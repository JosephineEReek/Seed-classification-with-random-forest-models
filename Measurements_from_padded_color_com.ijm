run("Set Measurements...", "area centroid center perimeter bounding fit shape feret's integrated median skewness kurtosis display redirect=None decimal=3");

// Directory (navigate in pop-upto folder containing padded color crops of individual seeds)
input=getDirectory("Choose Source Directory ");
list = getFileList(input);
for (i = 0; i < list.length; i++)
        Measurements(input, list[i]);
function Measurements (input,filename){
	open (input + filename);
	run("Set Scale...", "distance=600 known=25.4 unit=mm global");

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

	setOption("BlackBackground", false);
	run("Convert to Mask");
//Measure stuff
	roiManager("reset");
	run("Analyze Particles...", "size=2-Infinity show=Outlines display include summarize");
	close();
	
// Save results as csv
   name = "Measurements"; 
   index = lastIndexOf(name, "\\"); 
   if (index!=-1) name = substring(name, 0, index); 
   name = name + ".csv"; //can change xls to csv, txt, etc.
   saveAs("Measurements_from_color", dir+name);  

}


