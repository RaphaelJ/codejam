#include <cstdio>
#include <cstring>
#include <iostream>

using namespace std;

enum quaternion_t {
    One         = 0,
    MinusOne    = 1,
    I           = 2,
    MinusI      = 3,
    J           = 4,
    MinusJ      = 5,
    K           = 6,
    MinusK      = 7
};

// quaternion_t intToQuaterion(int i) 
// {
//     switch (a) {
//     case 0:         return One;
//     case 1:         return MinusOne;
//     case 2:         return I;
//     case 3:         return MinusI;
//     case 4:         return J;
//     case 5:         return MinusJ;
//     case 6:         return K;
//     case 7:         return MinusK;
//     };
// }
// 
// int quaterionToInt(quaternion_t q) 
// {
//     switch (q) {
//     case One:       return 0;
//     case MinusOne:  return 1;
//     case I:         return 2;
//     case MinusI:    return 3;
//     case J:         return 4;
//     case MinusJ:    return 5;
//     case K:         return 6;
//     case MinusK:    return 7;
//     };
// }

quaternion_t neg(quaternion_t a)
{
    switch (a) {
    case One:       return MinusOne;
    case MinusOne:  return One;
    case I:         return MinusI;
    case MinusI:    return I;
    case J:         return MinusJ;
    case MinusJ:    return J;
    case K:         return MinusK;
    case MinusK:    return K;
    };
}

quaternion_t mult(quaternion_t a, quaternion_t b)
{
    switch (a) {
    case One:       return b;
    case MinusOne:  return neg(b);
    case I:
        switch (b) {
        case One:       return I;
        case I:         return MinusOne;
        case J:         return K;
        case K:         return MinusJ;
        case MinusOne:  return MinusI;
        case MinusI:    return One;
        case MinusJ:    return MinusK;
        case MinusK:    return J;
        };
    case MinusI:        return neg(mult(I, b));
    case J:
        switch (b) {
        case One:       return J;
        case I:         return MinusK;
        case J:         return MinusOne;
        case K:         return I;
        case MinusOne:  return MinusJ;
        case MinusI:    return K;
        case MinusJ:    return One;
        case MinusK:    return MinusI;
        };
    case MinusJ:        return neg(mult(J, b));
    case K:
        switch (b) {
        case One:       return K;
        case I:         return J;
        case J:         return MinusI;
        case K:         return MinusOne;
        case MinusOne:  return MinusK;
        case MinusI:    return MinusJ;
        case MinusJ:    return I;
        case MinusK:    return One;
        };
    case MinusK:        return neg(mult(K, b));
    };
}

quaternion_t charToQuaternion(char c)
{
    switch (c) {
    case 'i': return I;
    case 'j': return J;
    case 'k': return K;
    };
}

quaternion_t *to_quaternion(const char str[], int len_str)
{
    quaternion_t *str2 = new quaternion_t[len_str];

    for (int i = 0; i < len_str; i++)
        str2[i] = charToQuaternion(str[i]);

    return str2;
}

quaternion_t *repeat_str(
    const quaternion_t str[], int len_str, int n_repeat, int *total_len
)
{
    *total_len = n_repeat * len_str;

    quaternion_t *repeated_str = new quaternion_t[*total_len];

    for (int i = 0; i < n_repeat; i++) {
        for (int j = 0; j < len_str; j++)
            repeated_str[i * len_str + j] = str[j];
    }

    return repeated_str;
}

bool resolve(const quaternion_t str[], int len_str)
{
    static const quaternion_t needle[]   = { I, J, K };
    static const int          len_needle = 3;

    quaternion_t cache[8][8];

    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 8; j++) {
            cache[i][j] = mult((quaternion_t) i, (quaternion_t) j);
        }
    }


    quaternion_t acc1 = One;

    for (int i = 0; i < len_str; i++) {
        acc1 = cache[acc1][str[i]];

        if (acc1 == I) {
            quaternion_t acc2 = One;

            for (int j = i + 1; j < len_str; j++) {
                acc2 = cache[acc2][str[j]];

                if (acc2 == J) {
                    quaternion_t acc3 = One;

                    for (int k = j + 1; k < len_str; k++) {
                        acc3 = cache[acc3][str[k]];

                        if (acc3 == K && k + 1 == len_str)
                            return true;
                    }
                }
            }
        }
    }

    return false;
}

// bool resolve_go(
//     const quaternion_t str[], int len_str, int i_str,
//     const quaternion_t needle[], int len_neddle, int i_needle
// )
// {
//     bool first = true;
//     quaternion_t acc;
// 
//     if (i_str == len_str && i_needle == len_neddle)
//         return true;
// 
//     for (; i_str < len_str; i_str++) {
//         fprintf(stderr, "%d\n", i_str);
// 
//         if (first) {
//             acc = str[i_str];
//             first = false;
//         } else
//             acc = mult(acc, str[i_str]);
// 
//         if (acc == needle[i_needle]) {
//             bool ret = resolve_go(
//                 str, len_str, i_str + 1, needle, len_neddle, i_needle + 1
//             );
// 
//             if (ret)
//                 return true;
//         }
//     }
// 
//     return false;
// }
// 
// bool resolve(const quaternion_t str[], int len_str)
// {
//     static const quaternion_t needle[]   = { I, J, K };
//     static const int          len_needle = 3;
// 
//     if (len_str == 0)
//         return false;
//     else {
//         return resolve_go(str, len_str, 0, needle, len_needle, 0);
//     }
// }

int main(void)
{
    int n_tests, len_str, n_repeat;
    scanf("%d", &n_tests);

    for (int i = 0; i < n_tests; i++) {
        scanf("%d %d", &len_str, &n_repeat);

        char *str = new char[len_str + 1];
        scanf("%s", str);
        quaternion_t *str2         = to_quaternion(str, len_str);
        delete str;

        int          total_len;
        quaternion_t *repeated_str = repeat_str(
            str2, len_str, n_repeat, &total_len
        );
        delete str2;

        if (resolve(repeated_str, total_len))
            printf("Case #%d: YES\n", i + 1);
        else
            printf("Case #%d: NO\n", i + 1);

        delete repeated_str;
    }

    return 0;
}
