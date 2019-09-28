typedef void *(*m_get)(value m, symbol b);
typedef void *(*m_set)(value m, symbol b, value v);
typedef u64 (*m_elements)(value m);
typedef string (*m_format)(heap h, value m);
typedef void (*each)(value k, value v, thunk next);
typedef void (*m_iterate)(heap h, value v, each e);

typedef struct methods {
    m_get get;
    m_set set;
    m_iterate iterate;
    m_format format;
    m_elements element;                
} *methods;

#define get(__m, __k) (methods(__m)->get)(__m, __k)
#define set(__m, __k, __v) (methods(__m)->set)(__m, __k, __v)
#define elements(__m) (methods(__m)->elements)(__m)
#define format(__h, __m)  (methods(__m)->format)(__h, __m)
#define iterate(__h, __m, __e) (methods(__m)->iterate)(__h, __m, __e)

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

