import lib

def bit_stuff(data: str):
    ones_cnt = 0
    output = ""
    
    for bit in data:
        output += bit

        if bit == '1':
            ones_cnt += 1
            if ones_cnt == 5:  # bit stuffing
                output += '0'
                ones_cnt = 0
        else:
            ones_cnt = 0
    
    return output

def encode(data: str):
    output = ""
    frames_count = 0

    for start in range(0, len(data), 32):
        frame_data = data[start : start + 32]

        print(frame_data)

        frame_data += lib.crc(frame_data)

        
        frame_data = bit_stuff(frame_data)

        output += '01111110'  # start of frame
        output += frame_data
        output += '01111110'  # end of frame

        frames_count += 1

    print("frames_count = " + str(frames_count))
    return output

if __name__ == "__main__":
    with open('Z', 'r') as f:
        in_data = f.read().strip()

    out_data = encode(in_data)

    with open('W', "w") as f:
        f.write(out_data)

