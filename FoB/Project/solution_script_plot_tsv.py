"""
    Description:
    Drawing ROC-Plot from tsv files

     It should be run specifing input files (tsv files)
     -ititle(optional), (-ititle title_string), with argument title will be added to plot with title_string

     python3 plot_tsv.py -itsv polyphen_2020_small_output_xy.tsv -itsv sift_2020_small_output_xy.tsv -itsv baseline_2020_small_output_xy.tsv -o output.

     with title:
    python3 plot_tsv.py -itsv polyphen_2020_small_output_xy.tsv -itsv sift_2020_small_output_xy.tsv -itsv baseline_2020_small_output_xy.tsv -ititle 2020_small -o output.

"""

import argparse
import numpy
import matplotlib
import matplotlib.pyplot as plt

def parse_tsv_file(filename):
    """
    Parse tsv file
    :param filename: input file with tsv files
    :return: list of x coordinators and list of y coordinators
    """

    global type_vep
    if 'sift' in filename:
        type_vep = 'sift'
    elif 'polyphen' in filename:
        type_vep = 'polyphen'
    elif 'baseline' in filename:
        type_vep = 'BLOSUM'
    else:
        type_vep = ''

    x_coordinators = []
    y_coordinators = []

    with open(filename, 'r') as f:
        for line in f:
            line = line.rstrip()
            arr = line.split("\t")

            if len(arr) != 2:
                print("Warning: the following line does not have two elements separated by a tab:\n", line)
            x_coordinators.append(float(arr[0]))
            y_coordinators.append(float(arr[1]))

    return x_coordinators, y_coordinators


def integrate(x, y):
    """
    Calculate the Area Under the Curve (AUC) for a given list of coordinates
    :param x: a list of x-coordinates
    :param y: a list of y-coordinates
    :return: a float with the surface area under the curve described by x and y
    """

    auc = 0.
    last_x = x[0]
    last_y = y[0]
    for cur_x, cur_y in list(zip(x, y))[1:]:
        #########################
        ### START CODING HERE ###
        #########################
        h = cur_x - last_x
        a, b = last_y, cur_y
        trapezoid_Area = h*(a+b)/2
        auc = auc + trapezoid_Area

        #########################
        ###  END CODING HERE  ###
        #########################
        last_x = cur_x
        last_y = cur_y
    return auc

def roc_plot_together(list_x, list_y,labels,png_filename, title):
    """
       Draw roc plot for three line in one plot without color

       :param x: list x coordinators
       :param y: list y coordinators
       :param label: labels of each line
       """
    lw = 1
    list_color = ['g','r','m']
    figure, axes = plt.subplots(1, 1)
    for x,y,color,label in zip(list_x,list_y, list_color, labels):
        auc = integrate(x, y)
        line_label = '{} (AUC= {:.3f})'.format(label, auc)
        axes.plot(x, y,c=color, label = line_label)

    axes.plot((0, 1), (0, 1), '--', color='navy', lw=lw, linestyle  ='--', label='Random')

    axes.set_xlim([-0.008, 1.008])
    axes.set_ylim([-0.008, 1.008])

    axes.legend()
    axes.set_title(title)
    axes.set_xlabel('False Positive Rate')
    axes.set_ylabel('True Positive Rate')
    plt.savefig(png_filename)


def main(tsv_files, png_file, title):
    if len(tsv_files) != 3:
        print('warining: In order to plot baseline, sift, polyphen all together, please input three files by add -itsv before each of file name')
    else:
        labels = []
        list_x = []
        list_y = []
        for coordinator_list in tsv_files:
            x_coordinators, y_coordinators = parse_tsv_file(coordinator_list)
            labels.append(type_vep)
            list_x.append(x_coordinators)
            list_y.append(y_coordinators)
        roc_plot_together(list_x, list_y, labels, png_file, title)



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Draw and save a ROC plot to a file from tsv files")
    parser.add_argument("-itsv", "--tsv_file_of_coordinator", help="tab-separated tsv results file", action='append',
                        required=True)
    parser.add_argument("-ititle", "--plot_title", help="title_of_plot", required=False)
    parser.add_argument("-o", "--output_png", help="output png file", required=True)

    args = parser.parse_args()

    tsv_files = args.tsv_file_of_coordinator

    png_file = args.output_png

    title = args.plot_title

    main(tsv_files, png_file, title)
