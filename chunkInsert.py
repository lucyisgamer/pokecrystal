from pathlib import Path
import sys
import re

# constants
NUM_ARGS_SUPPORTED = 4 # x, y, input file, output directiory
MAP_SIZE = 64 # width and height in chunks
HEADER_SIZE = 8 # size in bytes
BANK_SIZE = 16384
NUM_HEADERS_IN_BANK = BANK_SIZE / HEADER_SIZE
HEADER_START_BANK = 0x80
CHUNK_START_BANK = 0xC0
CHUNK_LAST_BANK = 0xFF # please god don't let me use all of these banks i need these for stuff and things


numArgs = len(sys.argv)

if numArgs == 1: # python adds a dummy argument as the first one
    print("Usage: chunkInsert.py inputFile outputDirectory chunkX chunkY")
    print("chunkX and chunkY are zero based and index from the top left of the map")
    print("inputFile should be a binary file with the blockdata organized left to right, top to bottom")
    print("The bit depth of the block data is detected automatically from the imput size")
    print("Supported bit depths are 1, 2, 4, and 8 bits ber block")
    print("Chunks are automatically put into the correct files in outputDirectory")
    print("Important constants are specified within the script, changing these may break existing chunk data!")
    exit()

if numArgs != NUM_ARGS_SUPPORTED + 1: 
    print("Error - expected " + str(NUM_ARGS_SUPPORTED) + " arguments, got " + str(numArgs - 1))
    exit()


inputFilePath = sys.argv[1]
outputFolderPath = sys.argv[2]
chunkX = int(sys.argv[3])
chunkY = int(sys.argv[4])

headerOffset = (chunkX + (chunkY * MAP_SIZE)) * HEADER_SIZE # calculate the header bank and offset with that bank using the power of math
headerBank = headerOffset // BANK_SIZE
headerOffset = headerOffset % BANK_SIZE # oh how i missed having actual math operators

header = bytearray(HEADER_SIZE)

with open(Path(inputFilePath), "rb") as input:
    inputData = input.read()

blocksAddress = 0
blocksBank = 0
reusedData = False
fitChunk = 0

for i in range(CHUNK_LAST_BANK - CHUNK_START_BANK):
    filePath = Path(outputFolderPath + "/chunks" + str(i) + ".bin")
    print(i)
    if filePath.is_file():
        inFile = open(filePath, "rb")
    else:
        inFile = open(filePath, "x")
        inFile.close()
        inFile = open(filePath, "rb")
    fileData = inFile.read()
    potentialMatch = re.search(b"" + inputData, fileData)
    if potentialMatch: # the chunk already exists within this bank
        blocksAddress = potentialMatch.start()
        blocksBank = i
        reusedData = True
        fitChunk = 1
        inFile.close()
        break # we can now write the data out
    else:
        if BANK_SIZE - len(fileData) >= len(inputData): # the chunk doesn't exist within this bank and the bank can fit it
            blocksAddress = len(fileData)
            blocksBank = i
            fitChunk = 2
            inFile.close()
            break # we are done
        else:
            inFile.close()
            continue

if fitChunk == 0:
    print("Error - not enough space in ROM for chunk")
    exit()

print(fitChunk)

header[0] = blocksBank + CHUNK_START_BANK
header[1] = blocksAddress // 256
header[2] = blocksAddress % 256
# TODO - put the requisite bits into header[1] to mark the bit depth

outputHeaderPath = Path(outputFolderPath + "/headers" + str(headerBank) + ".bin")
outputHeaderFile = open(outputHeaderPath, "rb")
outputHeaderData = bytearray(outputHeaderFile.read())
outputHeaderFile.close()
outputHeaderFile = open(outputHeaderPath, "wb")

if len(outputHeaderData) < headerOffset + HEADER_SIZE:
    outputHeaderData.extend(bytearray(headerOffset + HEADER_SIZE - len(outputHeaderData)))

for i in range(HEADER_SIZE - 1):
    outputHeaderData[headerOffset + i] = header[i]

outputHeaderFile.write(outputHeaderData)
outputHeaderFile.close()

if reusedData:
    print("Success! Chunk refrence added to bank " + f'{(blocksBank + CHUNK_START_BANK):02x}' + " at offset " + f'{blocksAddress:04x}')
    exit() # we don't need to write any chunk data, we can just point to it with the header

outputChunkPath = Path(outputFolderPath + "/chunks" + str(blocksBank) + ".bin")
outputChunkFile = open(outputChunkPath, "rb")
outputChunkData = bytearray(outputChunkFile.read())
outputChunkFile.close()
outputChunkFile = open(outputChunkPath, "wb")

outputChunkData.extend(bytearray(inputData))
outputChunkFile.write(outputChunkData)

outputChunkFile.close()
print("Success! Chunk inserted into bank " + f'{(blocksBank + CHUNK_START_BANK):02x}' + " at offset " + f'{blocksAddress:04x}')