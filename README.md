# Plate_ReadWrite
 Format IQue data using platemap and annotated meta data

Inputs
- Platemap, which has spatially mapped out unique identifiers or index values
- Annotated Metadata, which contains additional info about each sample that is organized by its unique identifier or index #
- IQue Experimental Data, which is in the long format
- Data key, which describes the secondaries, beadsets, and sample arrangements used in each experimental plate alongside the antigens that correspond to each bead region
- Other Parameters
![image](https://user-images.githubusercontent.com/100164917/182453547-f8e065ae-369a-4f2f-b3a1-6ccc9869a053.png)

Formatting Requirements
- Platemap - one empty row between plates, rows must be labeled A:H or A:P, and columns must be labeled 1:12 or 1:24
- Annotated Metadata - must contain a unique_id column (case does not matter) and there must be no duplicates in the unique_id column
- IQue data - must be in long format and contain a Well.ID column
- data key - must contain plate, set, platemap, secondary, region, and antigen columns (case does not matter)
  - plate column (experimental plate #)
  - set (type of bead used)
  - platemap (plate # that matches the platemap used)
  - secondary (type of secondary reagent used)
  - region (bead region)
  - antigen (antigen that corresponds to the beadregion)


The Process
![image](https://user-images.githubusercontent.com/100164917/182449781-91e54f0c-cb13-43d8-9551-daa4a13172fc.png)

![image](https://user-images.githubusercontent.com/100164917/182449859-7ecb52af-7c9c-40c2-b919-5c7b4f012931.png)

You can always contact me with questions at maanasa_mendu@college.harvard.edu or mmendu1@mgh.harvard.edu
