#!/bin/bash
../tools/bastok/bastok < src/IMTOD.bas > bin/IMTOD
../tools/bastok/bastok < src/IDTOM.bas > bin/IDTOM
ls -l bin
