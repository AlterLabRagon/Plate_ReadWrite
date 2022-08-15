To use Plate_ReadWrite you can download the User_friendly_version_new.R script. You can find example data and an annotated R Markdown Guide in the examples section. 

# Plate_ReadWrite
Objective: Format iQue flow cytometer data using platemap and annotated meta data
![image](https://user-images.githubusercontent.com/100164917/184568156-caa248a3-5f60-43e8-9291-43a33bcef967.png)

Inputs
- Platemap, which has spatially mapped out unique identifiers or index values
- Sample annotation data, which contains additional info about each sample that is organized by its unique identifier or index #
- IQue Experimental Data, which is in the long format
- Plate annotation data, which describes the secondaries, beadsets, and sample arrangements used in each experimental plate
- Other Parameters
![image](https://user-images.githubusercontent.com/100164917/184569590-55701cbe-e937-418c-ad5e-26e8e44d23b1.png)
![image](https://user-images.githubusercontent.com/100164917/184569598-ba3e164b-49a4-461c-8b34-75b34d796ebf.png)


Formatting Requirements
- Platemap - one empty row between plates, rows must be labeled A:H or A:P, and columns must be labeled 1:12 or 1:24
- Sample annotation - must contain a unique_id column (case does not matter) and there must be no duplicates in the unique_id column
- IQue data - must be in long format and contain a Well.ID column
- Plate annotation - must contain plate, set, platemap, secondary, region, and antigen columns (case does not matter)
  - plate column (experimental plate #)
  - set (type of bead used)
  - platemap (plate # that matches the platemap used)
  - secondary (type of secondary reagent used)

The Process
![image](https://user-images.githubusercontent.com/100164917/184567924-6222fe49-99b4-43d3-ad26-f5ab8fae613d.png)

![image](https://user-images.githubusercontent.com/100164917/182449859-7ecb52af-7c9c-40c2-b919-5c7b4f012931.png)

Feel free to contact me with questions at mmendu1@mgh.harvard.edu.
