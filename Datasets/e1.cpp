#include<iostream>
#include<fstream>
#include<stdlib.h>
#include<bits/stdc++.h>
using namespace std;

int main()
{
int i,j;
float k;
ofstream out;
out.open("e15.arff");
float duration,c_pbeg,c_pend,c_sa,c_an,c_ha,e_pbeg,e_pmid,e_pend,e_ha,e_an,e_sa,policies;
int x,y,z;

out<<"@relation e15.arff";

out<<"\n@attribute duration(min) integer";
out<<"\n@attribute c_pbeg(Hz) integer";
out<<"\n@attribute c_pend(Hz) integer";
out<<"\n@attribute c_sa integer";
out<<"\n@attribute c_an integer";
out<<"\n@attribute c_ha integer";
out<<"\n@attribute e_pbeg(Hz) integer";
out<<"\n@attribute e_pmid(Hz) integer";
out<<"\n@attribute e_pend(Hz) integer";
out<<"\n@attribute e_sa integer";
out<<"\n@attribute e_an integer";
out<<"\n@attribute e_ha integer";
out<<"\n@attribute policies integer";



out<<"\n \n@data \n";

for(i=1;i<=300;i++)

{
     float a=rand()%26;
    out<<float(a/5)+0.1<<",";
 k=85+rand()%81;
 out<<k<<",";
 k=85+rand()%171;
 out<<k<<",";
 x=rand()%5;
 y=rand()%5;
 z=rand()%5;
 if(x+y+z==5)
 {
     out<<x<<",";
         out<<y<<",";
         out<<z<<",";

 }
 for(k=0;x+y+z!=5;++k)
 {
     x=rand()%5;
 y=rand()%5;
 z=rand()%5;
     if(x+y+z==5)
     {
         out<<x<<",";
         out<<y<<",";
         out<<z<<",";
     }
 }

 k=85+rand()%100;
 out<<k<<",";
 k=85+rand()%125;
 out<<k<<",";
 k=85+rand()%100;
 out<<k<<",";
 x=rand()%5;
 y=rand()%5;
 z=rand()%5;
 if(x+y+z==5)
 {
     out<<x<<",";
         out<<y<<",";
         out<<z<<",";

 }
 for(k=0;x+y+z!=5;++k)
 {
     x=rand()%5;
 y=rand()%5;
 z=rand()%5;
     if(x+y+z==5)
     {
         out<<x<<",";
         out<<y<<",";
         out<<z<<",";
     }
 }
 k=rand()%4;
 out<<k<<",";
 out<<"\n";

}
out.close();
return 0;
}
