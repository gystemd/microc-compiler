
void double_matrix(int a[][3] , int n){
    int i;
    int j;
    for(i = 0; i< n; i++){
        for(j=0;j < 3; j++){
            a[i][j]  *= 2;
        }
    }
}

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
    double_matrix(a,3);
    for(i = 0; i < 3; i++){
        for(j = 0; j < 3; j++){
            
            print(a[i][j]);
        }
    }
}