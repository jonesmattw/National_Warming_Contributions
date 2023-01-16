# National Warming Contributions

To run the code for the first time, follow these steps.

## Step 1: Clone GitHub Repo

Navigate to your chosen ROOT directory, then run:

git clone https://github.com/jonesmattw/National_Warming_Contributions.git

## Step 2: Download & Unzip Input Data from Zenodo Repo

Download Input.zip from https://zenodo.org/record/7076347

Save Input.zip to /PATH/TO/YOUR/ROOT/Input.zip

Unzip the input files in place. After unzipping, the files should be located at /PATH/TO/YOUR/ROOT/Input

The unzipped directory tree will already be organised as required for code to run.

## Step 3: Set Root Directory in SETUP.R

Set the ROOT variable as desired.

ROOT <- "/PATH/TO/YOUR/ROOT/"

## Step 4: Make the output directory tree 

Make the output directory structure. You can run the following code (linux/Mac) to create directories as required. Don't forget to set your own root.

$ROOT='/PATH/TO/YOUR/ROOT/DIRECTORY'

mkdir $ROOT/EMISSIONS

mkdir $ROOT/GMST

mkdir $ROOT/PUBLIC_DATA

mkdir $ROOT/PLOTS

## Step 5: Run code sequentially using GO.R

Run GO.R to run the scripts of the coding package in the intended order. 
