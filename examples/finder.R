client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
data <- email_finder(client, domain="tomba.io",fname="FNAME",lname="LNAME")
data
