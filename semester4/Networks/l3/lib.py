import binascii

def crc(data: str):
    int_arr = [int(data[i:i+8], 2) for i in range(0, len(data), 8)] 
    v = binascii.crc32(bytes(int_arr))
    return format(v, 'b').rjust(32, '0')

