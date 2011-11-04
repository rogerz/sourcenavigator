#include <iostream>
#include <string>

namespace test1 {

        int func(bool x)
        {
                return(!x);
        }

}


namespace test2 {

        int func(bool y)
        {
                return y;
        }
}

int main(int ac, char **dc)
{
        using namespace std;

        cout << "test1:: " << test1::func(true) << endl;
        cout << "test2:: " << test2::func(true) << endl;

        return 0;
}
