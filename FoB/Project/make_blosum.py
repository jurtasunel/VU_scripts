### This script reads in an MSA file in fasta format.
### Original Henikoff & Henikoff 1992 article: https://pmc.ncbi.nlm.nih.gov/articles/PMC50453/pdf/pnas01096-0363.pdf

### Import libraries.
import sys
import math

### Define functions.
def parse_fasta(input_file):
    """
    This function parses a Multiple Sequence Alignment file. The input is a
    fasta file with the aligned sequences and the output is a list of lists
    with format [[seqID1, seq1], [seqID2, seq2]... [seqIDn, seqn]].
    """
    # Open the file and read in the lines.
    with open(input_file, "r") as f:

        lines = f.readlines()
        # Make list to store the sequences.
        seqs = []

        # Loop through the lines
        for line in lines:

            # Add a new list for every new id with the format [ID, string].
            if line.strip()[0] == ">":
                seqs.append([line.strip(), ""])
                
            else:
                # Append the line to the string of the last list.
                seqs[-1][1] = seqs[-1][1] + line.strip()

    return seqs

def get_block(msa_list):
    """
    This function gets the longest block wihtout gaps from the Multiple Sequence
    Alignment list. The input is the output from the parse_fasta() function and
    the outputs are a list with the sequences from the longest block, and a list
    with all the start and en positios of all the blocks without gaps.
    """
    # Get length of each seq on the MSA.
    msalen = len(msa_list[0][1])

    # Make list to store positons of MSA without gaps.
    blocks = []
    # Loop through the positions on the MSA lists.
    for i in range(msalen):
        gap = 0 # Initialize gap count.
        # Loop through the sequences on the MSA list.
        for seq in range(len(msa_list)):
            # Increase the gap count if any of the sequences has a gap on that positon. 
            if msa_list[seq][1][i] == "-":
                gap = gap + 1
        # Add positions that don't have gaps on any sequence to the bloks list.
        if gap == 0:
            blocks.append(i)
    
    # Make list to store start and end position of each block.
    blocks_pos = []
    start, end = blocks[0], 0
    # Loop through the blocks positions.
    for i in range(len(blocks)-1):
        # Append start and end every time that a block finishes, and update the new start.
        if blocks[i] + 1 != blocks[i+1]:
            end = blocks[i]
            blocks_pos.append([start, end])
            start = blocks[i+1]

    # Add the last interval.
    blocks_pos.append([start, blocks[len(blocks)-1]])

    # Get the largest block.
    start, end = blocks_pos[0][0], blocks_pos[0][1]
    for i in range(len(blocks_pos)):
        if blocks_pos[i][1] - blocks_pos[i][0] > blocks_pos[i-1][1] - blocks_pos[i-1][0]:
            start, end = blocks_pos[i][0], blocks_pos[i][1]

    # Make list to store sequences of the largest block.
    block = []
    for seq in msa_list:
        block.append(seq[1][start:end+1])

    return block, blocks_pos

def count_frequencies(block):
    """
    This function calculates the frequencies of each AA and each AApair on the block.
    The input is the first output of the get_block() function and the outputs are a
    dictionary with the frequencies of each AA and a dictionary wit the frequencies
    of each AA pair.
    """
    # Count frequency of each Aminoacid.
    Faa = {}
    for i in range(len(block)):
        for j in range(len(block[0])):
            aa = str(block[i][j])
            if aa in Faa:
                Faa[aa] = Faa[aa] + 1
            else:
                Faa[aa] = 1

    # Count frequency of each Aminoacid pair.
    Fpairs = {}
    # Loop through the length of the sequences.
    for i in range(len(block[0])):
        # Loop through the Aminoacids on each column.
        for aa in range(len(block)-1):
            # For each aa, get list of pairs to compare and loop through each pair.
            pairs = list(range(aa+1,len(block)))
            for pair in pairs:
                # Get each pair.
                ipair = str(block[aa][i]) + str(block[pair][i])
                # Add count to the pair if it exist.
                if ipair in Fpairs:
                    Fpairs[ipair] = Fpairs[ipair] + 1
                # Check also the reverse pair (AB and BA are the same pair). 
                elif ipair[1]+ipair[0] in Fpairs:
                    rev = ipair[1]+ipair[0]
                    Fpairs[rev] = Fpairs[rev] + 1
                # Create new pair if it does not exist.
                else:
                    Fpairs[ipair] = 1
    
    return Faa, Fpairs

def calculate_scores(Faa, Fpairs):
    """
    This 
    """
    # Calculate Probability of occurence of each Aminoacid.
    ### On the paper, they calculate it using the Pairs frquencies. Here we do it with the AA frequencies. It's the same thing.. but easier to code!! =D
    Paa = {}
    for aa in Faa.keys():
        Paa[aa] = Faa[aa] / sum(Faa.values())

    # Calculate observed and expected probabilities of each AApair, and calculate the LogOdds value.
    Pobs = []
    Pexp = []
    Lod = {}

    #########################
    ### START CODING HERE ###
    #########################

    for pair in Fpairs:
        # Observed probability = Pair Frequency / Total pairs 
        obs = Fpairs[pair] / sum(Fpairs.values())
        Pobs.append(obs)

        # Expected probabilities for equal AA on the pair = Probaility of AA by Probability of AA.
        if pair[0] == pair[1]:
            exp = Paa[pair[0]] * Paa[pair[0]]
            Pexp.append(exp)
    
        # Expected probabilities for different AA on the pair = Probaility of AA1 by Probability of AA2 by 2 (the 2 account for AB and BA being the same pair).
        else:
            exp = Paa[pair[0]] * Paa[pair[1]] * 2
            Pexp.append(exp)

        # Calculate Lod (Log Odds scores) of each pair, which is the log2 ratio of Pobserved/Pexpected, and multiplied by a scaling factor of 2.
        lod = math.log(obs/exp, 2)*2
        Lod[pair] = round(lod)

    #######################
    ### END CODING HERE ###
    #######################

    return Lod

def writeout_matrix(LogOdds_dict):
    """
    This function
    """    
    # Make scafold of matrix. Add two spaces to match the BLOSUM62 file format from the FoB project.
    matrix = [["x", "  A", "  R", "  N", "  D", "  C", "  Q", "  E", "  G", "  H", "  I", "  L", "  K", "  M", "  F", "  P", "  S", "  T", "  W", "  Y", "  V"],
              ["A"] + ["  0"] * 20,
              ["R"] + ["  0"] * 20,
              ["N"] + ["  0"] * 20,
              ["D"] + ["  0"] * 20,
              ["C"] + ["  0"] * 20,
              ["Q"] + ["  0"] * 20,
              ["E"] + ["  0"] * 20,
              ["G"] + ["  0"] * 20,
              ["H"] + ["  0"] * 20,
              ["I"] + ["  0"] * 20,
              ["L"] + ["  0"] * 20,
              ["K"] + ["  0"] * 20,
              ["M"] + ["  0"] * 20,
              ["F"] + ["  0"] * 20,
              ["P"] + ["  0"] * 20,
              ["S"] + ["  0"] * 20,
              ["T"] + ["  0"] * 20,
              ["W"] + ["  0"] * 20,
              ["Y"] + ["  0"] * 20,
              ["V"] + ["  0"] * 20]

    # Loop through the matrix.
    for row in range(1, len(matrix)):
        for col in range(1, len(matrix[0])):
            # Get the Aa pair (the index 2 on the second pair is to get the AA without the first two spaces).
            pair = str(matrix[row][0]) + str(matrix[0][col][2])
            # Add the corresponding LogOdds value from each pair to the matrix.
            if pair in LogOdds_dict.keys():
                # Add two spaces for single digit values.
                if len(str(LogOdds_dict[pair])) == 1:
                    matrix[row][col] = "  " + str(LogOdds_dict[pair])
                # Add one space for two digits.
                elif len(str(LogOdds_dict[pair])) == 2:
                    matrix[row][col] = " " + str(LogOdds_dict[pair])
                # No spaces with three digits.
                elif len(str(LogOdds_dict[pair])) == 3:
                    matrix[row][col] = str(LogOdds_dict[pair])

    with open("testfile.txt", "w") as f:
        for row in matrix:
            line = "".join(map(str, row))
            f.write(line + "\n")


    return

### Run main funtion.
def main():
    # Get input file as terminal argument.
    largest_block = get_block(parse_fasta(sys.argv[1]))[0]
    Faa, Fpairs = count_frequencies(largest_block)
    print(writeout_matrix(calculate_scores(Faa, Fpairs)))

if __name__ == "__main__":
    main()