int main(){
    int a[3];
    a[0] = 0;
    a[1] = 1;
    a[2] = 2;
    int i = 0;
    a[i++] += 5;
    print(a[0]);
    print(a[i]);
}