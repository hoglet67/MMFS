#!/bin/bash

rm -rf build
mkdir -p build

# Set the BEEBASM executable for the platform
BEEBASM=tools/beebasm/beebasm.exe
if [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    BEEBASM=tools/beebasm/beebasm
fi

# Create a blank SSD image
tools/mmb_utils/blank_ssd.pl build/mmfs.ssd
echo

for top in  top_*.asm
do
    name=`echo ${top%.asm} | cut -c5-`
    echo "Building $name..."

    # Assember the ROM
    $BEEBASM -i ${top} -o build/${name} -v >& build/${name}.log

    # Check if ROM has been build, otherwise fail early
    if [ ! -f build/${name} ]
    then
        cat build/${name}.log
        echo "build failed to create ${name}"
        exit
    fi

    # Create the .inf file
    echo -e "\$."${name}"\t8000\t8000" > build/${name}.inf

    # Add into the SSD
    tools/mmb_utils/putfile.pl build/mmfs.ssd build/${name}

    # Report end of code
    grep "code ends at" build/${name}.log
    
    # Report build checksum
    echo "    mdsum is "`md5sum <build/${name}`
done

echo
tools/mmb_utils/info.pl  build/mmfs.ssd
