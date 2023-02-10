australia = open("Australia.txt","r")
belgium = open("Belgium.txt","r")




lines = belgium.readlines()
linesaus = australia.readlines() 
belgium_list = []
australia_list = []
for line in lines:
    belgium_list.append(line.strip())
for line in linesaus:
    australia_list.append(line.strip())
    
lista_def = []
for element in belgium_list:
    if element not in australia_list:
        lista_def.append(element[1:11])
        
print(lista_def)