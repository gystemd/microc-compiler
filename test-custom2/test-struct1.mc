struct s {
    int a;
    int b;
};

int main() {
    struct s f;
    f.a = 3;
    f.b = 4;
    print(f.a);
    f.a = 5;
    print(f.b);
    print(f.a);
    return 0;
}
