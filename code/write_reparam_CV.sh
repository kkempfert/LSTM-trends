filenum=240	  # Number of files to be created (should equal number of rows)
vararray1=()	# Array of first column values
vararray2=()	# Array of second column values
vararray3=()	# Array of third column values
vararray4=()
vararray5=()
vararray6=()
vararray7=()

# Recursively read csv file row-by-row
while IFS=, read -r ip1 ip2 ip3 ip4 ip5 ip6 ip7
do

	vararray1+=("$ip1")	# Store first column values
	vararray2+=("$ip2")	# Store second column values
	vararray3+=("$ip3")	# Store third column values
	vararray4+=("$ip4")
	vararray5+=("$ip5")
	vararray6+=("$ip6")
	vararray7+=("$ip7")

done < hyperparams_CV.csv

# Write variable values to python file
for(( i = 0 ; i < filenum ; i++ ))do

	# Replace line in template with new input values
	sed -i "s/param_neurons = .*/param_neurons = ${vararray1[i]}/" template_reparam_CV.py
	sed -i "s/param_station_name = .*/param_station_name = ${vararray2[i]}/" template_reparam_CV.py
	sed -i "s/param_num_K = .*/param_num_K = ${vararray3[i]}/" template_reparam_CV.py
  	sed -i "s/param_opt = .*/param_opt = ${vararray4[i]}/" template_reparam_CV.py
	sed -i "s/param_seed_i = .*/param_seed_i = ${vararray5[i]}/" template_reparam_CV.py
	sed -i "s/param_seed_b = .*/param_seed_b = ${vararray6[i]}/" template_reparam_CV.py
	sed -i "s/param_batch_size = .*/param_batch_size = ${vararray7[i]}/" template_reparam_CV.py

	# Write to a new file with file name stored in array
	cp template_reparam_CV.py file_reparam_CV$i.py

done


