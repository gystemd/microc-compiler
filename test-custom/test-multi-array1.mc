int main(){

    int a[3][3];
    int i;
    int j;
    int val = 1;
    for(i = 0; i < 3; i++){
        for(j = 0; j < 3; j++){
            a[i][j] = val++;
            print(a[i][j]);
        }
    }
}