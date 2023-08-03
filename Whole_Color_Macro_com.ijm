// This macro is slightly adapted it from here:
// https://zenodo.org/record/5595203#.Y153yezMLt1

// The code that allows direct saving into a csv. file is currently commented out. If small batches of images are analysed at
// a time, it can be used. If large bazches are used, the code will not be able to do all of them and it can be helpful to run
// it as far as it gets, then copy the output into an excel file and run again on the images that were not done in the first round.

// Macro that automates the extraction of color metrics (RGB, Lab, and HSB) from each image. 
// This macro functions to 1) open an image from a directory folder, 2) threshold the image to 
// segment out the object of interest from the background then "clear" the background so it is completely black, 
// 3) separate the image into the component channels for each color space, 4) extract summary statistics for each channel 
// in each colorspace, 5) add these summary statistics for each channel of each colorspace (RGB, Lab, and HSB) to a .csv file 

//Written by Chris Strock, 2021-09-30

//user selects working directory
input=getDirectory("Choose Source Directory ");
list = getFileList(input);
for (i = 0; i < list.length; i++)
        Wholecolor(input, list[i]);
function Wholecolor (input,filename){
	open (input + filename);

//open ROI manager
run("ROI Manager...");
roiManager("Show All");

// Assign raw opened image the name "currentImage"
currentImage=getImageID();

//Duplicate raw image to make a copy that will be modified to make a mask
run("Duplicate...", "title=Mask");

// Highlight the "Mask" image as the duplicate that will be thresholded to make the mask
selectWindow("Mask");

// Color Threshold "Mask" image to select only the object of interest
// NOTE: The thresholding parameters below will need to be adjusted for your specific image set
// Thresholding parameterization shown here is based on RGB colorspace, but thresholding based on other color spaces
// may be a better option for other image sets. Before running this macro, determine the best 
// thresholding colorspace and parameters for the image set you are working with and replace 
// the code below.

// Start of color thresholding parameterization:
min=newArray(3);
max=newArray(3);
filter=newArray(3);
a=getTitle();
run("RGB Stack");
run("Convert Stack to Images");
selectWindow("Red");
rename("0");
selectWindow("Green");
rename("1");
selectWindow("Blue");
rename("2");
min[0]=0;
max[0]=171; //originally 255
filter[0]="pass";
min[1]=0; 
max[1]=155; //originally 255
filter[1]="pass";
min[2]=0;
max[2]=150; //originally 136
filter[2]="pass";
for (i=0;i<3;i++){
  selectWindow(""+i);
  setThreshold(min[i], max[i]);
  run("Convert to Mask");
  if (filter[i]=="stop")  run("Invert");
}
imageCalculator("AND create", "0","1");
imageCalculator("AND create", "Result of 0","2");
for (i=0;i<3;i++){
  selectWindow(""+i);
  close();
}
selectWindow("Result of 0");
close();
selectWindow("Result of Result of 0");
rename(a);
// End of color thresholding parameterization-------------

//Turn the thresholded image into a binary image; need to specify 
//here that the background it black and need to "resetThreshold", 
//otherwise IJ gets confused when making a selection in the next step 
//If this step is not taken, sometimes it will select the object as the 
//ROI and sometimes it will select the background as the ROI.
setOption("BlackBackground", true); 
run("Make Binary");
resetThreshold();

//Create a selection = selecting the area (ROI) that will be measured
run("Create Selection");

//Transfer the selection from the "Mask" image to the original image
selectImage(currentImage);
run("Restore Selection");

// Blackout and clear the background (what is not being measured in the ROI) and save 
// this in the folder containing the images being analyzed. This saved image can be used for 
// QC. Post-analysis the user can quickly look back at these images to see if any errors occured.
setBackgroundColor(0, 0, 0);
run("Clear Outside");
//saveAs("Jpeg", input + filename);

//Duplicate masked image to make 3 copies that will used for extracting RGB, Lab and HSB values
run("Duplicate...", "title=RGB");
rename(filename+"_RGB");

run("Duplicate...", "title=LAB");
rename(filename+"_LAB");

run("Duplicate...", "title=HSB");
rename(filename+"_HSB");

//Break down the image into its composite channels (R, G, and B channels are separated)
selectWindow(filename+"_RGB");
run("Make Composite");
run("Restore Selection");
roiManager("Add");

//set the measurements in the ROI manager for what info will be recorded (label data with imageID, total pixel area analyzed, 
//mean, modal, SD, min, max gray values for each channel)
run("Set Measurements...", "area display mean standard modal min limit redirect=None decimal=3");

//use multimeasure to collect these data from each of the 3 composite channels (R, G, and B channels)
//and have data append the log file so as not to overwrite data from previous image
roiManager("multi-measure measure_all one append");

//clear ROI manager and Mask image so there is a clean workspace for the next image in the folder
roiManager("Deselect");
roiManager("Delete");
close(filename+"_RGB");

// Converts image pixels from sRGB color space to CIE L*a*b* color space. Uses fixed formulas described at http://www.brucelindbloom.com
// The output is a stack of floating point images (GRAY32). 
// L* band is in the range of 0 to 100, a* from -100 to 100, and b* from -100 to 100
selectWindow(filename+"_LAB");
run("Lab Stack");
run("Restore Selection");
roiManager("Add");

//set the measurements in the ROI manager for what info will be recorded (label data with imageID, total pixel area analyzed, 
//mean, modal, SD, min, max gray values for each channel)
run("Set Measurements...", "area display mean standard modal min limit redirect=None decimal=3");

//use multimeasure to collecte these data from each of the 3 composite channels (L, a*, and b*)
//and have data append the log file so as not to overwrite data from previous image
roiManager("multi-measure measure_all one append");

//clear ROI manager and Mask image so there is a clean workspace for the next image in the folder
roiManager("Deselect");
roiManager("Delete");
close(filename+"_LAB");


// Converts image pixels from sRGB color space to HSB color space. 
selectWindow(filename+"_HSB");
run("HSB Stack");
run("Restore Selection");
roiManager("Add");

//set the measurements in the ROI manager for what info will be recorded (label data with imageID, total pixel area analyzed, 
//mean, modal, SD, min, max gray values for each channel)
run("Set Measurements...", "area display mean standard modal min limit redirect=None decimal=3");

//use multimeasure to collecte these data from each of the 3 composite channels (H, S, and B channels)
//and have data append the log file so as not to overwrite data from previous image
roiManager("multi-measure measure_all one append");

//clear ROI manager and Mask image so there is a clean workspace for the next image in the folder
roiManager("Deselect");
roiManager("Delete");
close("Mask");
close(filename+"_HSB");

//save data collected by ROI manager to a .csv file titled "Whole_Color_Measurements.csv"
//this .csv file will be saved to the same folder the the images were originally stored in

//   dir=getDirectory("image");
   //Change the file name here
//   name = "Whole_Color_Measurements"; 
//   index = lastIndexOf(name, "\\"); 
//   if (index!=-1) name = substring(name, 0, index); 
//   name = name + ".csv"; ///can change xls to csv, txt, etc.
//   saveAs("Measurements", dir+name); 
   
//close();
}
//run("Clear Results");

