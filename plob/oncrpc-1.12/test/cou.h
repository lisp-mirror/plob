#include <rpc/types.h>

#define COUNTSERV ((u_long)77)
#define COUNTVERS ((u_long)1)
#define inc ((u_long)1)
#ifdef __cplusplus
extern "C" {
extern int *inc_1(...);
}
#else
extern int *inc_1();
#endif /* __cplusplus */
#define dec ((u_long)2)
#ifdef __cplusplus
extern "C" {
extern int *dec_1(...);
}
#else
extern int *dec_1();
#endif /* __cplusplus */
#define set ((u_long)3)
#ifdef __cplusplus
extern "C" {
extern int *set_1(...);
}
#else
extern int *set_1();
#endif /* __cplusplus */

