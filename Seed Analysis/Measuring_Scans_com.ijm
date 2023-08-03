// Directory (navigate in pop-up)
dir=getDirectory("Choose a data folder");
list = getFileList(dir);

// Access images
for (i = 0; i < list.length; i++)
        Measurements(dir, list[i]);
function Measurements (input,filename){

//open images
	run("Bio-Formats Importer", "open=" + dir+list[i] + " color_mode=Default view=Hyperstack");
// Crop edge, set general cropping parameters, scale,...
	makeRectangle(108, 60, 4908, 6888);
	run("Crop");
	main = getTitle();
	run("Set Scale...", "distance=600 known=25.4 unit=mm global");
// Thresholding
	setAutoThreshold("Default no-reset");
//run("Threshold...");
	setThreshold(0, 0);
	setOption("BlackBackground", false);
	run("Convert to Mask");

// Dilate
	selectWindow(main);
	run("Dilate");

// Analyze particles
	roiManager("reset");
	run("Analyze Particles...", "size=2-Infinity show=Outlines display include summarize");
	close();
	
// Save results as csv
   name = "Measurements"; 
   index = lastIndexOf(name, "\\"); 
   if (index!=-1) name = substring(name, 0, index); 
   name = name + ".csv"; ///can change xls to csv, txt, etc.
   saveAs("Measurements", dir+name);  
}

run("Clear Results");

