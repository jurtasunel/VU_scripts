#!/usr/bin/python3

"""
DESCRIPTION:
    Template code for the Dynamic Programming assignment in the Algorithms in Sequence Analysis course at the VU.
    
INSTRUCTIONS:
    Complete the code (compatible with Python 3!) upload to CodeGrade via corresponding Canvas assignment.

AUTHOR:
    <Name and student ID here!>
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
    M = len(seq1)+1 # M moves seq1, rows.
    N = len(seq2)+1 # N moves seq2, columnw.
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
        for i in range(1, len(score_matrix[0])):
            score_matrix[0][i] = score_matrix[0][i-1] - gap_penalty
        for i in range(1, len(score_matrix)):
            score_matrix[i][0] = score_matrix[i-1][0] - gap_penalty 
        #####################
        #  END CODING HERE  #
        #####################

    
    
    ### 2: Fill in Score Matrix
 
    #####################
    # START CODING HERE #
    #####################
    def dp_function(i,j):

        # Get the scores from the left, diagonal and top cells.
        lscore = score_matrix[i][j-1] -gap_penalty
        tscore = score_matrix[i-1][j] - gap_penalty
        dscore = score_matrix[i-1][j-1] + substitution_matrix[seq1[i-1]][seq2[j-1]]

        # Make values 0 if for local alignment.
        if strategy == "local":
           if lscore < 0:
            lscore = 0
           if tscore < 0:
            tscore = 0
           if dscore < 0:
            dscore = 0

        # Make variables to store the final score and the traceback direction.
        sc, tb = 0, ""
        # Implement high rode traceback direction.
        if max(lscore, tscore, dscore) == tscore:
            sc, tb = tscore, "t"
        elif max(lscore, tscore, dscore) == dscore:
            sc, tb = dscore, "d"
        else:
            sc, tb = lscore, "l"

        # Return the cell score and its traceback direction.
        return [sc, tb]
    
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
    
    aligned_seq1 = "" 
    aligned_seq2 = ""
    align_score = 0
    start = () # Make variable to store starting cell for each strategy.

    # Find the alignment score (which is also the starting cell for traceback) for each strategy:
    if strategy == "global": # Bottom right cell.
        align_score = score_matrix[M-1][N-1] 
        start = (M-1, N-1)

    elif strategy == "semiglobal": # Highest value in last row or column. If more than one, high road (priority is on right-most column and top-most row).
        for i in range(M): # only higher values in last column to keep top-most row index.
            if score_matrix[i][N-1] > align_score:
                align_score = score_matrix[i][N-1]
                start = (i, N-1)

        if max(score_matrix[M-1]) > align_score:
            for j in range(len(score_matrix[M-1])): # higher or equal values on last row to keep the right-most column.
                if score_matrix[M-1][j] >= align_score:
                    align_score = score_matrix[M-1][j]
                    start = (M-1, j)
        
        # Add required end gaps to seq 1 or 2 depenging on where the traceback start.
        if start[0] == (M-1):
            aligned_seq1 = ((N-1) - start[1]) * "-"
            aligned_seq2 = seq2[start[1]:len(seq2)]
        elif start[1] == (N-1):
            aligned_seq1 = seq1[start[0]:len(seq1)]
            aligned_seq2 = ((M-1) - start[0]) * "-"

    elif strategy == "local": # Highest value in matrix.
        start = (0,0)
        for i in range(len(score_matrix)):
            for j in range(len(score_matrix[i])):
                if score_matrix[i][j] > align_score or (score_matrix[i][j] == align_score and j > start[1]):
                    align_score = score_matrix[i][j]
                    start = (i,j)                

    # Do traceback from start cell.
    i = start[0]
    j = start[1]

    while i > 0 or j > 0:
        
        # Exception for soft zeros.
        if dp_function(i,j)[0] == 0 and strategy == "local":

            # Calculate the real values before the 0 transfomr of the dp formula.
            lscore = score_matrix[i][j-1] -gap_penalty
            tscore = score_matrix[i-1][j] - gap_penalty
            dscore = score_matrix[i-1][j-1] + substitution_matrix[seq1[i-1]][seq2[j-1]]
            # Do high road if one of the three calculations is a zero.
            if max(tscore, dscore, lscore) == 0:
                if tscore == 0:
                    aligned_seq1 = seq1[i-1] + aligned_seq1
                    aligned_seq2 = "-" + aligned_seq2
                    i = i-1
                elif dscore == 0:
                    aligned_seq1 = seq1[i-1] + aligned_seq1
                    aligned_seq2 = seq2[j-1] + aligned_seq2
                    i = i-1
                    j = j-1
                elif lscore == 0:
                    aligned_seq1 = "-" + aligned_seq1
                    aligned_seq2 = seq2[j-1] + aligned_seq2
                    j = j-1
            # Break if none of them is a real zero.
            else:
                break

        elif dp_function(i,j)[1] == "d":
            aligned_seq1 = seq1[i-1] + aligned_seq1
            aligned_seq2 = seq2[j-1] + aligned_seq2
            i = i-1
            j = j-1
        
        elif dp_function(i,j)[1] == "t":
            aligned_seq1 = seq1[i-1] + aligned_seq1
            aligned_seq2 = "-" + aligned_seq2
            i = i-1

        elif dp_function(i,j)[1] == "l":
            aligned_seq1 = "-" + aligned_seq1
            aligned_seq2 = seq2[j-1] + aligned_seq2
            j = j-1

        # If end of traceback is not on 0,0, add the required begining gaps.
        if (i == 0 or j == 0) and strategy != "local":
            if i == 0:
                aligned_seq1 = (len(seq2[0:j]) * "-") + aligned_seq1
                aligned_seq2 = seq2[0:j] + aligned_seq2

            if j == 0:
                aligned_seq1 = seq1[0:i] + aligned_seq1
                aligned_seq2 = (len(seq1[0:i]) * "-") + aligned_seq2
            break
        elif (i == 0 or j == 0) and strategy == "local":
            break  

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
    print('\n'.join([seq1,match,seq2,'','Score = %i' % score]))



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
        print_score_matrix(seq1,seq2,score_matrix)
        print('') # Insert a blank line in between
        print_alignment(alignment)
    
    # Save results
    if args.align_out: save_alignment(alignment, args.align_out)
    if args.matrix_out: save_score_matrix(score_matrix, args.matrix_out)



if __name__ == '__main__':
    main()