int fact(int n) {
    if(n <= 0)
        return 1;
    else
        return n * fact(n-1);
}
int main() {
    print(fact(6));
    return 0;
}
