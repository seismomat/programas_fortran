import sys 
sys.path.append('../Finitas_tes')
import matplotlib.pyplot as plt
import numpy as np
from pp import read_dotDatafile 
data=np.array(read_dotDatafile("./herramientas/derivadas.dat"))


print(data[:,0].shape)
plt.plot(data[:,0],data[:,2])
plt.plot(data[:,0],data[:,1],color='black',linestyle='--')
plt.show()

"""
def f(x,v):
    v=1
    return (-g/l)*np.sin(x)

g=9.81;l=0.5;h=0.01
x=np.pi/5.0; v=0.0

x_l=[];v_l=[]

t=np.r_[1:1000:1]

for i in t:
    v=v+f(x,v)*h
    x=x+v*h 
    x_l.append(x)
    v_l.append(v)

"""

