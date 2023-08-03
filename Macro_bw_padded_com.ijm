// Cuts black and white scans with several seeds on them into individual images of the same size with one sees on each

// Directory
dir=getDirectory("Choose a data folder");
list = getFileList(dir);
processed_dir_name = dir + "Cropped" + File.separator;
File.makeDirectory(processed_dir_name);

// Batch
setBatchMode(true);
for (i=0; i<list.length; i++) {

//open images
	run("Bio-Formats Importer", "open=" + dir+list[i] + " color_mode=Default view=Hyperstack");
	name = File.getName(list[i]); //because dir only leads to the folder I can't get the idividual image names...

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
	run("Analyze Particles...", "size=2-Infinity show=Outlines display include summarize add");

// Cropping
	selectWindow(main);
	for (j=0; j<roiManager("count"); ++j) {
    	run("Duplicate...", "title=crop");
    	roiManager("Select", j);
    	run("Crop");
		run("Canvas Size...", "width=500 height=500 position=Center zero");
		saveAs("Tiff", processed_dir_name+name+j+".tif");
    	close();
     	//Next round!
     	selectWindow(main);
	}
}
