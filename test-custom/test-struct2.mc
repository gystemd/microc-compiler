struct complex{
    float real;
    float img;
};

struct complex add(struct complex n1, struct complex n2) {
    struct complex temp;
    temp.real = n1.real + n2.real;
    temp.img = n1.img + n2.img;
    return temp;
}

int main(){
    struct complex n1;
    struct complex n2;
    n1.real = 2.1;
    n1.img = 1.3;

    n2.real = .5;
    n2.img = .9;
    struct complex r = add(n1, n2);
    printfloat(r.real);
    printfloat(r.img);


}