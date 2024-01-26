./encoder_exe $1 encoded $2
./decoder_exe encoded decoded.tga $1
nomacs decoded.tga
