void print_string(char str[]){
    int i;
    for(i = 0; str[i] != '\0';i++){
        printchar(str[i]);
    }
    
}

int main(){
    print_string("hello, world!\n");
}