doedit "C:\Users\tuncay\OneDrive\PhD-CGU\382-econometrics1\practicum3\Do-File_Practicum3_Group_Exercise_041219.do"

gen ln_a3=ln(a3)
gen ln_a13=ln(a13)
gen ln_a16=ln(a16)
reg ln_a3 ln_a13 ln_a16 a22 a26 a29,r // restricted
reg ln_a3 ln_a13 ln_a16 a22 a26 a29 a46 a47 a48 a49 a50,r // unrestricted
test a46 a47 a48 a49 a50 // joint hypothesis of race variables
