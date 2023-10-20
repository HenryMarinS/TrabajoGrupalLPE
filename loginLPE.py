#!/usr/bin/env python
# coding: utf-8

# In[1]:


token = {
  "apiKey": "AIzaSyDZeu_FAc1gl7xwZiOn_TgeCyGwQZbbtpQ",
  "authDomain": "grupo4-lpe-941e0.firebaseapp.com",
  "databaseURL": "https://grupo4-lpe-941e0-default-rtdb.europe-west1.firebasedatabase.app",
  "projectId": "grupo4-lpe-941e0",
  "storageBucket": "grupo4-lpe-941e0.appspot.com",
  "messagingSenderId": "828559743389",
  "appId": "1:828559743389:web:0b41bf3cbdbfba05c43774",
  "measurementId": "G-CEQT1MFJFW"
};


# In[2]:


import pyrebase as pyre


# In[3]:


firebase = pyre.initialize_app(token)


# In[4]:


auth = firebase.auth()
db = firebase.database()
storage = firebase.storage()


# In[5]:


#Funcion para el registro
def registro(user,pwd):
        auth.create_user_with_email_and_password(user,pwd)


# In[6]:


#Funcion para el login
def login(user,pwd):
    try:
        auth.sign_in_with_email_and_password(user,pwd)
        print("Exito en el login")
    except:
        print("Error en el login")


# In[7]:


#Funcion para obtener la autenficacion del email
def getAuth(firebase):
    auth = firebase.auth()
    return auth


# In[8]:


#Funcion para definir datos del usuario
def datosUsuario (email,nombre,matricula,marca,modelo):
    data = {"email":email,"nombre":nombre,"matricula":matricula,"marca":marca,"modelo":modelo}
    db.child("users").push(data)


# In[ ]:




