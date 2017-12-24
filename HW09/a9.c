#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"

void *ktr_emptyr__m__k(void *dismount) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._dismount = dismount;
  return (void *)_data;
}

void *ktr_subr1r__m__k(void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1r__m__k_kt;
  _data->u._subr1r__m__k._k = k;
  return (void *)_data;
}

void *ktr_zeror__m__k(void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zeror__m__k_kt;
  _data->u._zeror__m__k._k = k;
  return (void *)_data;
}

void *ktr_multr__m__outerr__m__k(void *envr__m__cps, void *x, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__outerr__m__k_kt;
  _data->u._multr__m__outerr__m__k._envr__m__cps = envr__m__cps;
  _data->u._multr__m__outerr__m__k._x = x;
  _data->u._multr__m__outerr__m__k._k = k;
  return (void *)_data;
}

void *ktr_multr__m__innerr__m__k(void *v, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__innerr__m__k_kt;
  _data->u._multr__m__innerr__m__k._v = v;
  _data->u._multr__m__innerr__m__k._k = k;
  return (void *)_data;
}

void *ktr_ifr__m__k(void *envr__m__cps, void *conseq, void *alt, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _ifr__m__k_kt;
  _data->u._ifr__m__k._envr__m__cps = envr__m__cps;
  _data->u._ifr__m__k._conseq = conseq;
  _data->u._ifr__m__k._alt = alt;
  _data->u._ifr__m__k._k = k;
  return (void *)_data;
}

void *ktr_letr__m__k(void *envr__m__cps, void *body, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letr__m__k_kt;
  _data->u._letr__m__k._envr__m__cps = envr__m__cps;
  _data->u._letr__m__k._body = body;
  _data->u._letr__m__k._k = k;
  return (void *)_data;
}

void *ktr_throwr__m__outerr__m__k(void *envr__m__cps, void *kr__m__exp) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throwr__m__outerr__m__k_kt;
  _data->u._throwr__m__outerr__m__k._envr__m__cps = envr__m__cps;
  _data->u._throwr__m__outerr__m__k._kr__m__exp = kr__m__exp;
  return (void *)_data;
}

void *ktr_throwr__m__innerr__m__k(void *v, void *envr__m__cps) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throwr__m__innerr__m__k_kt;
  _data->u._throwr__m__innerr__m__k._v = v;
  _data->u._throwr__m__innerr__m__k._envr__m__cps = envr__m__cps;
  return (void *)_data;
}

void *ktr_appr__m__outerr__m__k(void *envr__m__cps, void *rand, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__outerr__m__k_kt;
  _data->u._appr__m__outerr__m__k._envr__m__cps = envr__m__cps;
  _data->u._appr__m__outerr__m__k._rand = rand;
  _data->u._appr__m__outerr__m__k._k = k;
  return (void *)_data;
}

void *ktr_appr__m__innerr__m__k(void *v, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__innerr__m__k_kt;
  _data->u._appr__m__innerr__m__k._v = v;
  _data->u._appr__m__innerr__m__k._k = k;
  return (void *)_data;
}

void *envrr_empty() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_envr;
  return (void *)_data;
}

void *envrr_extendr__m__env(void *v, void *env) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extendr__m__env_envr;
  _data->u._extendr__m__env._v = v;
  _data->u._extendr__m__env._env = env;
  return (void *)_data;
}

void *closr_closure(void *body, void *envr__m__cps) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._body = body;
  _data->u._closure._envr__m__cps = envr__m__cps;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
exp = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_throw(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
env = (void *)envrr_empty();
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Fact 5: %d\n", (int)v);}

void applyr__m__closure()
{
clos* _c = (clos*)rator;
switch (_c->tag) {
case _closure_clos: {
void *body = _c->u._closure._body;
void *envr__m__cps = _c->u._closure._envr__m__cps;
exp = (void *)body;
env = (void *)envrr_extendr__m__env(rand,envr__m__cps);
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)env;
switch (_c->tag) {
case _empty_envr: {
fprintf(stderr, "unbound identifier");
 exit(1);
break; }
case _extendr__m__env_envr: {
void *re = _c->u._extendr__m__env._v;
void *envx = _c->u._extendr__m__env._env;
if((vari == 0)) {
  v = (void *)re;
pc = &applyr__m__k;

} else {
  env = (void *)envx;
vari = (void *)(void *)((int)vari - 1);
pc = &applyr__m__env;

}
break; }
}
}

void applyr__m__k()
{
kt* _c = (kt*)k;
switch (_c->tag) {
case _emptyr__m__k_kt: {
void *dismount = _c->u._emptyr__m__k._dismount;
_trstr *trstr = (_trstr *)dismount;
longjmp(*trstr->jmpbuf, 1);
break; }
case _subr1r__m__k_kt: {
void *kx = _c->u._subr1r__m__k._k;
k = (void *)kx;
v = (void *)(void *)((int)v - 1);
pc = &applyr__m__k;
break; }
case _zeror__m__k_kt: {
void *kx = _c->u._zeror__m__k._k;
k = (void *)kx;
v = (void *)(v == 0);
pc = &applyr__m__k;
break; }
case _multr__m__outerr__m__k_kt: {
void *envr__m__cps = _c->u._multr__m__outerr__m__k._envr__m__cps;
void *x = _c->u._multr__m__outerr__m__k._x;
void *kx = _c->u._multr__m__outerr__m__k._k;
exp = (void *)x;
env = (void *)envr__m__cps;
k = (void *)ktr_multr__m__innerr__m__k(v,kx);
pc = &valuer__m__ofr__m__cps;
break; }
case _multr__m__innerr__m__k_kt: {
void *vx = _c->u._multr__m__innerr__m__k._v;
void *kx = _c->u._multr__m__innerr__m__k._k;
k = (void *)kx;
v = (void *)(void *)((int)vx * (int)v);
pc = &applyr__m__k;
break; }
case _ifr__m__k_kt: {
void *envr__m__cps = _c->u._ifr__m__k._envr__m__cps;
void *conseq = _c->u._ifr__m__k._conseq;
void *alt = _c->u._ifr__m__k._alt;
void *kx = _c->u._ifr__m__k._k;
if(v) {
  exp = (void *)conseq;
env = (void *)envr__m__cps;
k = (void *)kx;
pc = &valuer__m__ofr__m__cps;

} else {
  exp = (void *)alt;
env = (void *)envr__m__cps;
k = (void *)kx;
pc = &valuer__m__ofr__m__cps;

}
break; }
case _throwr__m__outerr__m__k_kt: {
void *envr__m__cps = _c->u._throwr__m__outerr__m__k._envr__m__cps;
void *kr__m__exp = _c->u._throwr__m__outerr__m__k._kr__m__exp;
exp = (void *)kr__m__exp;
env = (void *)envr__m__cps;
k = (void *)ktr_throwr__m__innerr__m__k(v,env);
pc = &valuer__m__ofr__m__cps;
break; }
case _throwr__m__innerr__m__k_kt: {
void *v = _c->u._throwr__m__innerr__m__k._v;
void *envr__m__cps = _c->u._throwr__m__innerr__m__k._envr__m__cps;
k = (void *)v;
pc = &applyr__m__k;
break; }
case _letr__m__k_kt: {
void *envr__m__cps = _c->u._letr__m__k._envr__m__cps;
void *body = _c->u._letr__m__k._body;
void *kx = _c->u._letr__m__k._k;
exp = (void *)body;
env = (void *)envrr_extendr__m__env(v,envr__m__cps);
k = (void *)kx;
pc = &valuer__m__ofr__m__cps;
break; }
case _appr__m__outerr__m__k_kt: {
void *envr__m__cps = _c->u._appr__m__outerr__m__k._envr__m__cps;
void *rand = _c->u._appr__m__outerr__m__k._rand;
void *kx = _c->u._appr__m__outerr__m__k._k;
exp = (void *)rand;
env = (void *)envr__m__cps;
k = (void *)ktr_appr__m__innerr__m__k(v,kx);
pc = &valuer__m__ofr__m__cps;
break; }
case _appr__m__innerr__m__k_kt: {
void *vx = _c->u._appr__m__innerr__m__k._v;
void *kx = _c->u._appr__m__innerr__m__k._k;
rand = (void *)v;
rator = (void *)vx;
k = (void *)kx;
pc = &applyr__m__closure;
break; }
}
}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)exp;
switch (_c->tag) {
case _const_expr: {
void *cexp = _c->u._const._cexp;
v = (void *)cexp;
pc = &applyr__m__k;
break; }
case _mult_expr: {
void *nexpr1 = _c->u._mult._nexpr1;
void *nexpr2 = _c->u._mult._nexpr2;
exp = (void *)nexpr1;
k = (void *)ktr_multr__m__outerr__m__k(env,nexpr2,k);
pc = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *nexp = _c->u._subr1._nexp;
exp = (void *)nexp;
k = (void *)ktr_subr1r__m__k(k);
pc = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *nexp = _c->u._zero._nexp;
exp = (void *)nexp;
k = (void *)ktr_zeror__m__k(k);
pc = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
exp = (void *)test;
k = (void *)ktr_ifr__m__k(env,conseq,alt,k);
pc = &valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
exp = (void *)body;
env = (void *)envrr_extendr__m__env(k,env);
pc = &valuer__m__ofr__m__cps;
break; }
case _throw_expr: {
void *kexp = _c->u._throw._kexp;
void *vexp = _c->u._throw._vexp;
exp = (void *)kexp;
k = (void *)ktr_throwr__m__outerr__m__k(env,vexp);
pc = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *expr = _c->u._let._exp;
void *body = _c->u._let._body;
exp = (void *)expr;
k = (void *)ktr_letr__m__k(env,body,k);
pc = &valuer__m__ofr__m__cps;
break; }
case _var_expr: {
void *n = _c->u._var._n;
vari = (void *)n;
pc = &applyr__m__env;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
v = (void *)closr_closure(body,env);
pc = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
exp = (void *)rator;
k = (void *)ktr_appr__m__outerr__m__k(env,rand,k);
pc = &valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
k= (void *)ktr_emptyr__m__k(dismount);
for(;;) {
pc();
}
}
return 0;
}
