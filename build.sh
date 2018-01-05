#!/bin/bash

rm -rf build
mkdir -p build

VERSION=`grep '#VERSION#' VERSION.asm | cut -d\" -f2`
echo "Building MMFS $VERSION"

# Set the BEEBASM executable for the platform
BEEBASM=tools/beebasm/beebasm.exe
if [ "$(uname -s)" == "Darwin" ]; then
    BEEBASM=tools/beebasm/beebasm-darwin
    MD5SUM=md5
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    BEEBASM=tools/beebasm/beebasm
    MD5SUM=md5sum
fi

# Device:
# M is MemoryMapped IO based (typically &FE18, for BeebEm)
# U is normal User Port VIA based
for device in U T E M
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
        filelist=top_E*.asm
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

        # Assember the ROM
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
