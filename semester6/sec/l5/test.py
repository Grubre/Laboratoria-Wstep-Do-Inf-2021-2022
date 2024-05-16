from bitarray import bitarray

def KSA(K):
    n = len(K)
    j = 0
    T = [K[i%n] for i in range(256)]
    S = list(range(256))
    for i in range(256):
        j = (j + S[i] + T[i]) % 256
        S[i], S[j] = S[j], S[i]
    return S

def PRGA(S, m):
    i = 0
    j = 0
    KS = []
    for _ in range(m):
        i = (i + 1) % 256
        j = (j + S[i]) % 256
        S[i], S[j] = S[j], S[i]
        KS.append(S[(S[i] + S[j]) % 256])
    return KS

key = bitarray('10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000')
S = KSA(key)
text = "Hello, World!"
KS = PRGA(S, len(text))

C = []
for i in range(len(text)):
    C.append(KS[i] ^ ord(text[i]))

print(f"encrypted text: {C}")

C2 = []
for i in range(len(text)):
    C2.append(KS[i] ^ C[i])

C2 = ''.join([chr(c) for c in C2])

print(f"decrypted text: {C2}")
