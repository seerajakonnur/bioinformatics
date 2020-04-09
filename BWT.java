import java.util.*;
import java.io.*;
import java.lang.*;

class A
{
 public static void main(String []args)
 {
 int i,j,temp;
 int z=1;
 int counter=0;
 Scanner s=new Scanner(System.in);
 System.out.println("enter the string with last character as $");
 String s1=s.nextLine();
 int l=s1.length();
 int []b=new int[l];
 char []a=s1.toCharArray();
 System.out.println("The character array is : ");
 for(i=0;i<l;i++)
  {
  System.out.println(a[i]);
  }

// First part is indexing the array
 System.out.print(a[0]+"0");
 b[0]=0;
 if(a[0]==a[1])
 {
 System.out.print(a[1]+"1");
 b[1]=1;
 }
 else
 {
 System.out.print(a[1]+"0");
 b[1]=0;
 }

 for(i=2;i<l;i++)
  {
  counter=0;
  for(j=0;j<i;j++)
  {
  if(a[i]==a[j])
  {
  counter=counter+1;
  }
  else
  {
  counter=counter+0;
  }
 }
 b[i]=counter;
 System.out.print(a[i]);
 //counter=b[i];
 System.out.print(counter);
 System.out.print("\n");
}

for(i=0;i<l;i++)
 {
 System.out.println(b[i]);
 }
//  Rotations of the array

 System.out.println("put into 2d array"); 
char [][]p=new char[l][l];
char [][]p1=new char[l][l];
String []s2=new String[l];

	for(i=0;i<l;i++)
 	{
	p[0][i]=a[i];
	}
	for(i=1;i<l;i++)
	{
	 for(j=0;j<l;j++)
	 {
	 if(j==(l-1))
	   {
	   temp=l;
	   }
	 else
	   {
	   temp=0;
	   }
	 p[i][j]=p[i-1][(j+1)-temp];
	 }
	}
	System.out.println("the matrix is :");
	for(i=0;i<l;i++)
	 {
	  System.out.println(" Rotation "+ z);
	  z++;
	  for(j=0;j<l;j++)
	  {
	   System.out.println(p[i][j]);
	  }
 	 }

	
	// convert array of arrays to string
	System.out.println(" The converted string array is : ");
	for(i=0;i<l;i++)
	{
	s2[i]=String.valueOf(p[i]);
	System.out.println(s2[i]);
	}

	//sorting arrays with $ having position 1

	Arrays.sort(s2);
	System.out.println(Arrays.toString(s2)); 
	
	for(i=0;i<l;i++)
	{
	p1[i]=s2[i].toCharArray();
	}

	System.out.println("the  transform matrix is : ");
 	
	for(i=0;i<l;i++)
	{
 	System.out.println(p1[i][(l-1)]);
 	}

	
 }
};
 
