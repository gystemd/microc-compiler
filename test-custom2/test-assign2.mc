int main() {
    float i;

    i = 0.0;

    i += i + 10.;
    printfloat(i);

    i += i + 10.e+2;
    printfloat(i);

    i -= i + .5;
    printfloat(i);

    i *= i + 2.0;
    printfloat(i);

    i *= i + 2.2E-1;
    printfloat(i);

    i /= i + 2.;
    printfloat(i);

    return 0;
}
