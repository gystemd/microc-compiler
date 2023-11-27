int foo(int i[]){
    print(i[0]);
}
int a = 3; //OK
char c = 'c'; //ok
int main()
{
    int a[3];
    a[0] = 1;
    foo(a);
}