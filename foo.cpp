// IN php I can use a
// class (type)
// namespace
// constant
// function
#include <iostream>
#include <streambuf>
#include <istream>
#include <ostream>

namespace My {
  int foo = 1;

  class Rectangle {
    int width, height;
  public:
    void set_values (int,int);
    int area (void);
  } rect;
}

// using namespace std;


// void Rectangle::set_values (int x, int y) {
//   width = x;
//   height = y;
// }

// class Square {
//     int side;
//   public:
//     void set_values (int,int);
//     int area (void);
// } sq;

// int mainnno() {
//   Rectangle *rectangle;
//   rectangle->set_values(1, 1);
//   cout << "area: " << rectangle->area();
//   return 0;
// }

// namespace foo {
//   namespace bar {
//     class MyClassInBar {
//     }
//     namespace baz {
//       int qux = 42;
//       class MyClassInBaz {
//       }
//     }
//   }
// }

// // ("fbz" type
// // (:kind alias :members
// //        (("foo::bar::baz" type
// //          (:type "namespace")
// //          nil nil))
// //        :type "namespace")
// // (:filename "/Users/andreaturso/Dev/my-semantic-php/foo.cpp")
// // #<overlay from 257 to 287 in foo.cpp>)
//

// MyClass instance = new MyClass();

// (dolist (tag (semantic-fetch-tags))
//  (when (eq 'type (semantic-tag-class tag)) (pp tag)))

// int main()
// {
//   std::cout << fbz::qux << '\n';
// }
