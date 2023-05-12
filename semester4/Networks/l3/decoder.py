import lib

def unstuff_and_decode(bits: str):
    ones_cnt = 0
    frame_buffer = ""
    decoded_output = ""

    frame_number = 0

    while bits:
        current_bit = bits[0]

        bits = bits[1:]

        if current_bit == '1':
            ones_cnt += 1
            if ones_cnt >= 7:
                frame_buffer = ""
            else:
                frame_buffer += current_bit
        else:
            if ones_cnt == 6:
                frame_buffer = frame_buffer[:-(7)]

                if frame_buffer:
                    data, crc_check = frame_buffer[:-32], frame_buffer[-32:]

                    calculated_crc = lib.crc(data)
                    if crc_check == calculated_crc:
                        print(f"{data}")
                        frame_number += 1
                        decoded_output += data
                    else:
                        print("Found wrong crc!")

                frame_buffer = ""
            elif ones_cnt == 5:
                pass 
            else:
                frame_buffer += current_bit 

            ones_cnt = 0

    print("frames count = " + str(frame_number))

    return decoded_output


if __name__ == "__main__":
    with open('W', 'r') as input_file:
        encoded_data = input_file.read().strip()

    decoded_data = unstuff_and_decode(encoded_data)

    with open('decoded', "w") as output_file:
        output_file.write(decoded_data)

