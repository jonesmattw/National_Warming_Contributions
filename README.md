# National Warming Contributions

To run the code for the first time, follow these steps.

## Step 1: Clone GitHub Repo

Navigate to your chosen ROOT directory.

git clone https://github.com/jonesmattw/National_Warming_Contributions.git

## Step 2: Download & Unzip Input Data from Zenodo Repo

https://zenodo.org/record/7076347

Unzipped Input file should be at {ROOT}/Input

The sub-directory tree will be as required for code to run.

## Step 3: Set Root Directory in SETUP.R

Replace the following line as appropriate:

ROOT <- "/Volumes/LaCie8/National_Warming_Contributions/"

## Step 4: Set Root Directory in SETUP.R

Make the output directory structure. You can run the following code (linux/Mac) to create directories as required. Don't forget to set your own root.

$ROOT='PATH/TO/YOUR/ROOT/DIRECTORY'

mkdir $ROOT/EMISSIONS

mkdir $ROOT/GMST

mkdir $ROOT/PUBLIC_DATA

mkdir $ROOT/PLOTS

## Step 5: Run code sequentially using GO.R

Run GO.R to run the scripts of the coding package in the intended order. 
