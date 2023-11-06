#include <stdio.h>
#include <stdlib.h>


int getint(){
  int n;
  scanf("%d",&n);
  return n;
}
float getfloat(){
  float f;
  scanf("%f",&f);
  return f;
}

char getcharacter(){
  char c;
  scanf(" %c",&c);
  return c;
}

void printchar(char c){
  printf("%c",c);
}

void printfloat(float f){
  printf("%f\n",f);
}

void print(int n){
  printf("%d\n", n);
}