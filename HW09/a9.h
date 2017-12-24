void (*pc)();

void *exp, *env, *k, *rator, *rand, *v, *vari;

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_body; void *_envr__m__cps; } _closure;
  } u;
};

void *closr_closure(void *body, void *envr__m__cps);

struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _empty_envr,
    _extendr__m__env_envr
  } tag;
  union {
    struct { char dummy; } _empty;
    struct { void *_v; void *_env; } _extendr__m__env;
  } u;
};

void *envrr_empty();
void *envrr_extendr__m__env(void *v, void *env);

struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _emptyr__m__k_kt,
    _subr1r__m__k_kt,
    _zeror__m__k_kt,
    _multr__m__outerr__m__k_kt,
    _multr__m__innerr__m__k_kt,
    _ifr__m__k_kt,
    _letr__m__k_kt,
    _throwr__m__outerr__m__k_kt,
    _throwr__m__innerr__m__k_kt,
    _appr__m__outerr__m__k_kt,
    _appr__m__innerr__m__k_kt
  } tag;
  union {
    struct { void *_dismount; } _emptyr__m__k;
    struct { void *_k; } _subr1r__m__k;
    struct { void *_k; } _zeror__m__k;
    struct { void *_envr__m__cps; void *_x; void *_k; } _multr__m__outerr__m__k;
    struct { void *_v; void *_k; } _multr__m__innerr__m__k;
    struct { void *_envr__m__cps; void *_conseq; void *_alt; void *_k; } _ifr__m__k;
    struct { void *_envr__m__cps; void *_body; void *_k; } _letr__m__k;
    struct { void *_envr__m__cps; void *_kr__m__exp; } _throwr__m__outerr__m__k;
    struct { void *_v; void *_envr__m__cps; } _throwr__m__innerr__m__k;
    struct { void *_envr__m__cps; void *_rand; void *_k; } _appr__m__outerr__m__k;
    struct { void *_v; void *_k; } _appr__m__innerr__m__k;
  } u;
};

void *ktr_emptyr__m__k(void *dismount);
void *ktr_subr1r__m__k(void *k);
void *ktr_zeror__m__k(void *k);
void *ktr_multr__m__outerr__m__k(void *envr__m__cps, void *x, void *k);
void *ktr_multr__m__innerr__m__k(void *v, void *k);
void *ktr_ifr__m__k(void *envr__m__cps, void *conseq, void *alt, void *k);
void *ktr_letr__m__k(void *envr__m__cps, void *body, void *k);
void *ktr_throwr__m__outerr__m__k(void *envr__m__cps, void *kr__m__exp);
void *ktr_throwr__m__innerr__m__k(void *v, void *envr__m__cps);
void *ktr_appr__m__outerr__m__k(void *envr__m__cps, void *rand, void *k);
void *ktr_appr__m__innerr__m__k(void *v, void *k);

void valuer__m__ofr__m__cps();
void applyr__m__k();
void applyr__m__env();
void applyr__m__closure();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

