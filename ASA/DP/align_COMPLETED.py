#!/usr/bin/python3

"""
DESCRIPTION:
    Template code for the Dynamic Programming assignment in the Algorithms in Sequence Analysis course at the VU.
    
INSTRUCTIONS:
    Complete the code (compatible with Python 3!) upload to CodeGrade via corresponding Canvas assignment.

AUTHOR:
    Bas Stringer, 1706462
"""



import argparse
import pickle



def parse_args():
    "Parses inputs from commandline and returns them as a Namespace object."

    parser = argparse.ArgumentParser(prog = 'python3 align.py',
        formatter_class = argparse.RawTextHelpFormatter, description =
        '  Aligns the first two sequences in a specified FASTA\n'
        '  file with a chosen strategy and parameters.\n'
        '\n'
        'defaults:\n'
        '  strategy = global\n'
        '  substitution matrix = pam250\n'
        '  gap penalty = 2')
        
    parser.add_argument('fasta', help='path to a FASTA formatted input file')
    parser.add_argument('output', nargs='*', 
        help='path to an output file where the alignment is saved\n'
        '  (if a second output file is given,\n'
        '   save the score matrix in there)')
    parser.add_argument('-v', '--verbose', dest='verbose', action='store_true',
        help='print the score matrix and alignment on screen', default=False)
    parser.add_argument('-s', '--strategy', dest='strategy',
        choices=['global','semiglobal','local'], default="global")
    parser.add_argument('-m', '--matrix', dest='substitution_matrix',
        choices=['pam250','blosum62','identity'], default='pam250')
    parser.add_argument('-g', '--gap_penalty', dest='gap_penalty', type=int,
        help='must be a positive integer', default=2)

    args = parser.parse_args()

    args.align_out = args.output[0] if args.output else False
    args.matrix_out = args.output[1] if len(args.output) >= 2 else False
                      # Fancy inline if-else statements. Use cautiously!
                      
    if args.gap_penalty <= 0:
        parser.error('gap penalty must be a positive integer')

    return args



def load_substitution_matrix(name):
    "Loads and returns the specified substitution matrix from a pickle (.pkl) file."
    # Substitution matrices have been prepared as nested dictionaries:
    # the score of substituting A for Z can be found with subst['A']['Z']
    # NOTE: Only works if working directory contains the correct folder and file!
    
    with open('substitution_matrices/%s.pkl' % name, 'rb') as f:
        subst = pickle.load(f)
    return subst
    
    

def load_sequences(filepath):
    "Reads a FASTA file and returns the first two sequences it contains."
    
    seq1 = []
    seq2 = []
    with open(filepath,'r') as f:
        for line in f:
            if line.startswith('>'):
                if not seq1:
                    current_seq = seq1
                elif not seq2:
                    current_seq = seq2
                else:
                    break # Stop if a 3rd sequence is encountered
            else:
                current_seq.append(line.strip())
    
    if not seq2:
        raise Exception('Error: Not enough sequences in specified FASTA file.')
    
    seq1 = ''.join(seq1)
    seq2 = ''.join(seq2)
    return seq1, seq2



def align(seq1, seq2, strategy, substitution_matrix, gap_penalty):
    "Do pairwise alignment using the specified strategy and parameters."
    # This function consists of 3 parts:
    #
    #   1) Initialize a score matrix as a "list of lists" of the appropriate length.
    #      Fill in the correct values for the first row and column given the strategy.
    #        (local / semiglobal = 0  --  global = stacking gap penalties)
    #   2) Fill in the rest of the score matrix using Dynamic Programming, accounting
    #      for the selected alignment strategy, substitution matrix and gap penalty.
    #   3) Perform the correct traceback routine on your filled in score matrix.
    #
    # Both the resulting alignment (sequences with gaps and the corresponding score)
    # and the filled in score matrix are returned as outputs.
    #
    # NOTE: You are strongly encouraged to think about how you can reuse (parts of)
    #       your code between steps 2 and 3 for the different strategies!
    
    
    ### 1: Initialize
    M = len(seq1)+1
    N = len(seq2)+1
    score_matrix = []
    for i in range(M):
        row = []
        score_matrix.append(row)
        for j in range(N):
            row.append(0)
    
    if strategy == 'global':
        #####################
        # START CODING HERE #
        #####################
        score_matrix[0] = [-gap_penalty * j for j in range(N)] # First row
        for i in range(M):                                     # First column
            score_matrix[i][0] = -gap_penalty * i
        #####################
        #  END CODING HERE  #
        #####################
    
    
    ### 2: Fill in Score Matrix
 
    #####################
    # START CODING HERE #
    #####################
    def dp_function(i,j):
        S = substitution_matrix[seq1[i-1]][seq2[j-1]]
        G = -gap_penalty
        options = [(score_matrix[i  ][j-1] + G, 1),   # Left -- 1
                   (score_matrix[i-1][j-1] + S, 2),   # Diagonal -- 2
                   (score_matrix[i-1][j  ] + G, 3)]   # Top -- 3
        if strategy == 'local': options.append((0,0)) # Local -- 0
        # Return a tuple containing the score, and what direction that score came from.
        # NOTE: max() on a list of tuples picks the one with the highest value in the first position,
        #   then breaks ties by the highest value in the second position, et cetera. "Top" having the
        #   highest value among the directions is how this is implementation takes the High Road alignment!
        return max(options)
    
    for i in range(1,M):
        for j in range(1,N):
            score_matrix[i][j] = dp_function(i,j)[0]
    #####################
    #  END CODING HERE  #
    #####################   
    
    
    ### 3: Traceback
    
    #####################
    # START CODING HERE #
    #####################   
    aligned_seq1 = ''
    aligned_seq2 = ''
    
    # FIND THE STARTING POINT
    if strategy == 'global':
        start = (M-1,N-1)
        
    elif strategy == 'semiglobal':
        # Check (first occurence of) highest score in last column
        _max = _i = -1
        for i,row in enumerate(score_matrix):
            if row[-1] > _max:
                _max = row[-1]
                _i = i

        # Check if last row has a higher score
        if max(score_matrix[-1]) > _max:  # If so, trace back from (M,N) to its column
            _max = max(score_matrix[-1])
            _j = N-1
            while score_matrix[-1][_j] != _max:
                _j -= 1
                aligned_seq1 += '-'
                aligned_seq2 = seq2[_j] + aligned_seq2
            start = (M-1,_j)
                
        else:                             # If not, trace back from (M,N) to row _i
            for i in range(M,_i+1,-1):
                aligned_seq1 = seq1[i-2] + aligned_seq1
                aligned_seq2 += '-'
            start = (_i,N-1)

    else: # strategy == 'local'
        _max = max([max(row) for row in score_matrix])
        start = False
        for j in range(N-1,-1,-1):
            for i in range(M):
                if score_matrix[i][j] == _max:
                    start = (i,j)
                    break
            if start: break

    # SAVE THE SCORE
    i = start[0]
    j = start[1]
    align_score = score_matrix[i][j]
    
    # TRACE BACK FROM START TO FINISH
    while i>0 and j>0:
        d = dp_function(i,j)[1] # Find the direction this score came from

        if d == 0:              # Stop Local alignment
            break
            
        elif d == 1:            # From the left
            j -= 1
            aligned_seq1 = '-' + aligned_seq1
            aligned_seq2 = seq2[j] + aligned_seq2

        elif d == 2:            # Diagonally
            i -= 1
            j -= 1
            aligned_seq1 = seq1[i] + aligned_seq1
            aligned_seq2 = seq2[j] + aligned_seq2

        elif d == 3:            # From the top
            i -= 1
            aligned_seq1 = seq1[i] + aligned_seq1
            aligned_seq2 = '-' + aligned_seq2            
    
    if strategy != 'local':
        aligned_seq1 = '-' * j + seq1[:i] + aligned_seq1
        aligned_seq2 = '-' * i + seq2[:j] + aligned_seq2
    #####################
    #  END CODING HERE  #
    #####################


    alignment = (aligned_seq1, aligned_seq2, align_score)
    return (alignment, score_matrix)



def print_score_matrix(s1,s2,mat):
    "Pretty print function for a score matrix."
    
    # Prepend filler characters to seq1 and seq2
    s1 = '-' + s1
    s2 = ' -' + s2
    
    # Print them around the score matrix, in columns of 5 characters
    print(''.join(['%5s' % aa for aa in s2])) # Convert s2 to a list of length 5 strings, then join it back into a string
    for i,row in enumerate(mat):               # Iterate through the rows of your score matrix (and keep count with 'i').
        vals = ['%5i' % val for val in row]    # Convert this row's scores to a list of strings.
        vals.insert(0,'%5s' % s1[i])           # Add this row's character from s2 to the front of the list
        print(''.join(vals))                   # Join the list elements into a single string, and print the line.



def print_alignment(a):
    "Pretty print function for an alignment (and alignment score)."
    
    # Unpack the alignment tuple
    seq1 = a[0]
    seq2 = a[1]
    score = a[2]
    
    # Check which positions are identical
    match = ''
    for i in range(len(seq1)): # Remember: Aligned sequences have the same length!
        match += '|' if seq1[i] == seq2[i] else ' ' # Fancy inline if-else statement. Use cautiously!
            
    # Concatenate lines into a list, and join them together with newline characters.
    print('\n'.join([seq1,match,seq2,'','Score: %i' % score]))



def save_alignment(a,f):
    "Saves two aligned sequences and their alignment score to a file."
    with open(f,'w') as out:
        out.write(a[0] + '\n') # Aligned sequence 1
        out.write(a[1] + '\n') # Aligned sequence 2
        out.write('Score: %i' % a[2]) # Alignment score


    
def save_score_matrix(m,f):
    "Saves a score matrix to a file in tab-separated format."
    with open(f,'w') as out:
        for row in m:
            vals = [str(val) for val in row]
            out.write('\t'.join(vals)+'\n')
    


def main(args = False):
    # Process arguments and load required data
    if not args: args = parse_args()
    
    sub_mat = load_substitution_matrix(args.substitution_matrix)
    seq1, seq2 = load_sequences(args.fasta)

    # Perform specified alignment
    strat = args.strategy
    gp = args.gap_penalty
    alignment, score_matrix = align(seq1, seq2, strat, sub_mat, gp)

    # If running in "verbose" mode, print additional output
    if args.verbose:
        print('')
        print_score_matrix(seq1,seq2,score_matrix)
        print('')
        print_alignment(alignment)
        print('')
    
    # Save results
    if args.align_out: save_alignment(alignment, args.align_out)
    if args.matrix_out: save_score_matrix(score_matrix, args.matrix_out)



if __name__ == '__main__':
    main()