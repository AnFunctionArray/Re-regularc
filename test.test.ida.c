extern int printf(const char*,...);

int main() {
    int a, b, c;

    c = 7;
    b = 2;
    (a = 4 && 0) || b++ && c++;

    return printf("a %d, b %d c %d\n", a || b && 0, b, c);
}