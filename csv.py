belgium = open("C:/Users/jpber/OneDrive/Documents/Belgica.txt","r")
chile = open("C:/Users/jpber/OneDrive/Documents/chile.txt","r")
lines = belgium.readlines()
linesch = chile.readlines() 
belgium_list = []
chile_list = []
for line in lines:
    belgium_list.append(line.strip())
for line in linesch:
    chile_list.append(line.strip())
    
lista_def = []
for element in belgium_list:
    if element not in chile_list:
        lista_def.append(element)
        
print(lista_def)