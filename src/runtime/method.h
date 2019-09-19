
// 64 bits per entry
#define GET_SLOT 0
#define ELEMENTS_SLOT 1
#define FORMAT_SLOT 2
#define ITERATE_SLOT 3
#define SET_SLOT 4 // warning - but it .. makes sense?
// consider using a va bit to distinguish between 'open' and 'finalized'
// objects...this can also work with compaction..kind of an s3 interface
#define METHOD_COUNT 5

extern function *map_methods;
//#define set_method(__t, __s, __m)  map_methods[METHOD_COUNT * __t + __s] = (function)__m

typedef void *(*m_get)(value m, symbol b);
typedef void *(*m_set)(value m, symbol b, value v);
typedef u64 (*m_elements)(value m);
typedef string (*m_format)(heap h, value m);
typedef void (*each)(value k, value v, thunk next);
// should this really call each instead of just returning it?
typedef void (*m_iterate)(heap h, value v, each e);

#define get(__m, __k) ((m_get)map_methods[tagof(__m)*METHOD_COUNT+GET_SLOT])(__m, __k)
#define set(__m, __k, __v) ((m_set)map_methods[tagof(__m)*METHOD_COUNT+SET_SLOT])(__m, __k, __v)
#define elements(__m) ((m_elements)map_methods[tagof(__m)*METHOD_COUNT+ELEMENTS_SLOT])(__m)
#define format(__h, __m)  ((m_format)map_methods[tagof(__m)*METHOD_COUNT+FORMAT_SLOT])(__h, __m)
#define iterate(__h, __m, __e) ((m_iterate)map_methods[tagof(__m)*METHOD_COUNT+ITERATE_SLOT])(__h, __m, __e)


static inline void iterator_each(value *pk, value *pv, thunk *pnext, 
                                 value k, value v, thunk next)
{
    *pk = k;
    *pv = v;
    *pnext = next;
}

#define foreach(__m, __k, __v)\
  for (void *__k = 0, *__v, *__pnext, *resume,\
           *it=specialize(transient, iterator_each, 6, &__k, &__v, &__pnext);__k != INVALID;) \
      for(iterate(transient, __m, it); __k != INVALID; ((thunk)__pnext)())

