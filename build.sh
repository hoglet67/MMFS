#!/bin/bash

rm -rf build
mkdir -p build

VERSION=`grep '#VERSION#' VERSION.asm | cut -d\" -f2`
echo "Building MMFS $VERSION"

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

# Device:
# U is normal User Port VIA based
# T is User Port connected "TurboMMC" interface
# E is Electron Plus One Printer Port connected interface (experimental)
# M is MemoryMapped IO based (typically &FE18, for BeebEm)
# P is Beeb Printer Port connected Interface (experimental)
for device in U T E M P
do
    build=build/${device}
    mkdir -p ${build}
    ssd=${build}/mmfs.ssd
    rm -f ${ssd}
    # Configure the device to be assembled
    echo "_DEVICE_ = '"$device"'" > DEVICE.asm
    # Create a blank SSD image
    tools/mmb_utils/blank_ssd.pl ${ssd}
    tools/mmb_utils/title.pl ${ssd} "MMFS $device $VERSION"
    echo

    if [ $device == "E" ]
    then
        filelist="top_E*.asm top_ZEMMFS.asm"
    elif [ $device == "P" ]
    then
        filelist="top_[MS]*.asm top_ZMMFS.asm"
    else
        filelist=top_*.asm
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

        # To save space, exclude the Z builds from the .ssd image
        if [[ $name != Z* ]]
        then
            # Create the .inf file
            echo -e "\$."${name}"\t8000\t8000" > ${build}/${name}.inf

            # Add into the SSD
            tools/mmb_utils/putfile.pl ${ssd} ${build}/${name}

            # Delete the .inf file
            rm -f ${build}/${name}.inf
        fi

        # Report end of code
        grep "code ends at" ${build}/${name}.log

        # Report build checksum
        echo "    mdsum is "`$MD5SUM <${build}/${name}`

        # Add a .rom suffix
        mv ${build}/${name} ${build}/${name}.rom
    done
    # Copy utilities
    tools/mmb_utils/putfile.pl ${ssd} utilities/bin/*
    echo
    tools/mmb_utils/info.pl  ${ssd}
done
rm -f DEVICE.asm
