#!/bin/bash

rm -rf build
mkdir -p build

if [ $# -eq 0 ]
then
    SYSTEMS="MMFS MMFS2"
else
    SYSTEMS=$(echo $* | tr '[:lower:]' '[:upper:]')
fi

## Sanity check systems
for system in $SYSTEMS
do
    if [ $system != "MMFS" ] && [ $system != "MMFS2" ]
    then
        echo "Unknown system: $system"
        exit
    fi
done



VERSION=`grep '#VERSION#' VERSION.asm | cut -d\" -f2`
echo "Building $SYSTEMS version $VERSION"

# Set the BEEBASM executable for the platform
# Let's see if the user already has one on their path
BEEBASM=$(type -path beebasm 2>/dev/null)
if [ "$(uname -s)" == "Darwin" ]; then
    BEEBASM=${BEEBASM:-tools/beebasm/beebasm-darwin}
    MD5SUM=md5
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    if [ "$(uname -m)" == "x86_64" ]; then
        BEEBASM=${BEEBASM:-tools/beebasm/beebasm64}
    else
        BEEBASM=${BEEBASM:-tools/beebasm/beebasm32}
    fi
    MD5SUM=md5sum
fi
BEEBASM=${BEEBASM:-tools/beebasm/beebasm.exe}
echo Using $BEEBASM


# Build the two different system:
#   MMFS is the original system that uses a BEEB.MMB file
#   MMFS2 is the more recent system that uses direct FAT32 access of .ssd/.dsd files
#
for system in $SYSTEMS
do

    # Device:
    # U is normal User Port VIA based
    # T is User Port connected "TurboMMC" interface
    # E is Electron Plus One Printer Port connected interface (experimental)
    # M is MemoryMapped IO based (typically &FE18, for BeebEm)
    # P is Beeb Printer Port connected Interface (experimental)
    # G is Mega Games Cartridge MKII

    if [ $system == "MMFS2" ]
    then
        DEVICES="U U2 U3 T T2 T3 E M P"
    else
        DEVICES="U U2 U3 T T2 T3 E M P G"
    fi

    for device in $DEVICES
    do
        build=build/${device}
        mkdir -p ${build}
        ssd=${build}/$(echo ${system} | tr '[:upper:]' '[:lower:]').ssd
        rm -f ${ssd}

        # Configure the system/device to be assembled
        echo "_DEVICE_ = \""$device"\""  > DEVICE.asm
        echo "MACRO SYSTEM_NAME"      >> DEVICE.asm
        echo "    EQUS \"${system}\"" >> DEVICE.asm
        echo "ENDMACRO"               >> DEVICE.asm
        if [ $system == "MMFS2" ]
        then
           echo "_MM32_      = TRUE"  >> DEVICE.asm
           echo "_MM32_DEBUG = FALSE" >> DEVICE.asm
           echo "_MM32_DDUMP = FALSE" >> DEVICE.asm
        else
           echo "_MM32_      = FALSE" >> DEVICE.asm
        fi

        # Create a blank SSD image
        tools/mmb_utils/blank_ssd.pl ${ssd}
        tools/mmb_utils/title.pl ${ssd} "$system $device $VERSION"
        echo

        if [ $device == "E" ]
        then
            filelist="top_E*.asm top_MAMMFS.asm top_MMFS.asm top_ZEMMFS.asm"
        elif [ $device == "G" ]
        then
            filelist="top_MGC*.asm"
        elif [ $device == "M" ]
        then
            filelist="top_MMFS.asm top_MMFSDBG.asm top_SWMMFS.asm top_SWMMFS+.asm top_MAMMFS.asm"
        elif [ $device == "P" ]
        then
            filelist="top_MMFS*.asm top_MAMMFS*.asm top_SWMMFS*.asm top_ZMMFS.asm"
        else
            filelist="top_*MMFS*.asm"
        fi

        for top in $filelist
        do
            name=`echo ${top%.asm} | cut -c5-`

#        if [ \
#            "${device}/${name}" == "T/SWMMFS" -o \
#            "${device}/${name}" == "T/ZMMFS" \
#            ]
#        then
#            echo "Skipping ${device}/$name due to space constraints"
#            continue;
#        else
            echo "Building ${device}/$name..."
#        fi

            # Assemble the ROM
            $BEEBASM -i ${top} -o ${build}/${name} -v >& ${build}/${name}.log

            # Check if ROM has been build, otherwise fail early
            if [ ! -f ${build}/${name} ]
            then
                cat ${build}/${name}.log
                echo "build failed to create ${device}/${name}"
                exit
            fi

            # Create the .inf file
            echo -e "\$."${name}"\t8000\t8000" > ${build}/${name}.inf

            # Add into the SSD
            tools/mmb_utils/putfile.pl ${ssd} ${build}/${name}

            # Delete the .inf file
            rm -f ${build}/${name}.inf

            # Report end of code
            grep "code ends at" ${build}/${name}.log

            # Report build checksum
            echo "    mdsum is "`$MD5SUM <${build}/${name}`

            # Add a .rom suffix
            mv ${build}/${name} ${build}/${name}.rom
        done
        # Copy utilities
        if [ $system = MMFS ]; then
          tools/mmb_utils/putfile.pl ${ssd} utilities/bin/IDTOM
          tools/mmb_utils/putfile.pl ${ssd} utilities/bin/IMTOD
        else
          tools/mmb_utils/putfile.pl ${ssd} utilities/bin/IDTOM2
          tools/mmb_utils/putfile.pl ${ssd} utilities/bin/IMTOD2
        fi
        echo
        tools/mmb_utils/info.pl  ${ssd}

        rm -f DEVICE.asm

    done # for device in G U T E M P

    # Move the files in the build directory to the right system folder
    # It would be cleaner to build in the correct directory in the first place
    # but ZMMFS sources contain absolute build paths so this is simpler.
    mkdir -p build/${system}
    for device in $DEVICES
    do
        mv build/${device} build/${system}
    done

done # for system in mmfs mmfs2
