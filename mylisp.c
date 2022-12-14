#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <setjmp.h>
#define TAILCALL return
#if defined __has_attribute
#	if __has_attribute(musttail)
#		undef TAILCALL
#		define TAILCALL __attribute__((musttail)) return
#	endif
#endif

#ifdef HAVE_LIBEDIT
#	include <histedit.h>
#endif

struct pair {
	void *car, *cdr;
};
#define ATOM_TAG ((void*)(intptr_t)-2)
#define NUM_TAG ((void*)(intptr_t)-4)

#define HEAP1_MAX (32*1024)
#define HEAP2_MAX (16*1024)
struct pair heap[HEAP1_MAX+HEAP2_MAX];
uint8_t heap2bm[HEAP2_MAX/8];

#define CAR(p) (((struct pair*)p)->car)
#define CDR(p) (((struct pair*)p)->cdr)
// NOTE: This is not the same as the atom proc. This only detects non-NIL symbols.
#define ATOM(p) (CAR(p) == ATOM_TAG)
#define NUMBER(p) (CAR(p) == NUM_TAG)

#define CAAR(p) CAR(CAR(p))
#define CADR(p) CAR(CDR(p))
#define CDAR(p) CDR(CAR(p))
#define CDDR(p) CDR(CDR(p))
#define CADDR(p) CAR(CDR(CDR(p)))
#define CADAR(p) CAR(CDR(CAR(p)))
#define CDADR(p) CDR(CAR(CDR(p)))

void *free1_list = NULL;
void *free2_list = NULL;
void *atoms_list = NULL;
void *evlis_list = NULL;

void *atom_pname = NULL;
void *atom_expr = NULL;
void *atom_fexpr = NULL;
void *atom_subr = NULL;
void *atom_fsubr = NULL;
void *atom_apval = NULL;
void *atom_label = NULL;
void *atom_lambda = NULL;
void *atom_funarg = NULL;
void *atom_quote = NULL;
void *atom_function = NULL;
void *atom_cond = NULL;
void *atom_prog = NULL;
void *atom_nil = NULL;
void *atom_f = NULL;
void *atom__t_ = NULL;
void *atom_t = NULL;
void *atom_trace = NULL;

int debug = 0;

/* Error handling */
jmp_buf jbuf;
int jbuf_inited = 0;
#define ERROR() do { if (!jbuf_inited) abort(); else longjmp(jbuf, 1); } while(0)

/* GC */
void **gc_roots[64];
int gc_nroots = 0;
int gc_watermark = 0;
void gc_push(void **p)
{
	if (!p)
		abort();
	if (gc_nroots == 64) {
		fprintf(stderr, "OUT OF GCROOTS\n");
		ERROR();
	}
	gc_roots[gc_nroots++] = p;
	// TODO: remove this
	if (gc_nroots > gc_watermark) {
		fprintf(stderr, "gc_nroots = %d\n", gc_nroots);
		gc_watermark = gc_nroots;
	}
}
void gc_pop()
{
	gc_nroots--;
}
/* General rules for GC:
 * Functions which call cons take and return dangling values.
 * For other functions it doesn't matter, since they don't
 * trigger gc cycle.
 * When calling cons, everything that's not reachable from the
 * resulting cons cell should be protected using gc_push.
 * There's a limited number of gc_push slots, so you shouldn't
 * recurse while you have protected values. Evlis is the only
 * function in the interpreter that's truly recursive, so it
 * pushes the values onto the global evlis_list, which is
 * always protected. Why not keep the entire gc_roots in a
 * single list? Because pushing to the list requires using cons,
 * and the most you can protect with that is two values:
 *     ls = cons(cons(a, b), ls);
 * In evlis we actually need to protect 3 values, hence why
 * we gc_push one of them before creating the list.
 */

#define MARK(c) ((void*)((intptr_t)(c) | 1))
#define UNMARK(c) ((void*)((intptr_t)(c) & ~(intptr_t)1))
#define ISMARK(c) ((intptr_t)(c) & 1)
void gc_mark(void *c)
{
	while (c) {
		if ((struct pair *)c >= &heap[HEAP1_MAX]) {
			int ofs = (struct pair *)c - &heap[HEAP1_MAX];
			heap2bm[ofs>>3] |= 1<<(ofs&7);
			return;
		}
		void *cc = CAR(c);
		if (ISMARK(cc))
			return;
		CAR(c) = MARK(cc);
		if (cc != ATOM_TAG && cc != NUM_TAG)
			gc_mark(cc);
		if (cc == NUM_TAG)
			break;
		c = CDR(c);
	}
}
void gc_stats()
{
	int free1 = 0, free2 = 0;
	for (void *p = free1_list; p; p = CDR(p))
		free1++;
	for (void *p = free2_list; p; p = CDR(p))
		free2++;
	fprintf(stderr, "PRE-GC  STATS: free1=%d free2=%d\n", free1, free2);
}
#if 0
void gc_check(void *p)
{
	if (!p)
		return;
	if ((struct pair *)p >= &heap[HEAP1_MAX]) {
		fprintf(stderr, "heap2 in gc_check\n");
		return;
	}
	memset(heap2bm, 0, sizeof(heap2bm));
	for (int i = 0; i < gc_nroots; i++)
		gc_mark(*gc_roots[i]);
	if (!ISMARK(CAR(p)))
		fprintf(stderr, "unmarked in gc_check\n");
	for (int i = 0; i < HEAP1_MAX; i++)
		CAR(&heap[i]) = UNMARK(CAR(&heap[i]));
}
#endif
void gc_collect()
{
	gc_stats();
	int free1 = 0, free2 = 0;
	memset(heap2bm, 0, sizeof(heap2bm));
	for (int i = 0; i < gc_nroots; i++) {
		gc_mark(*gc_roots[i]);
	}
	free1_list = NULL;
	for (int i = 0; i < HEAP1_MAX; i++) {
		if (ISMARK(CAR(&heap[i]))) {
			CAR(&heap[i]) = UNMARK(CAR(&heap[i]));
		} else {
			CDR(&heap[i]) = free1_list;
			free1_list = &heap[i];
			free1++;
		}
	}
	free2_list = NULL;
	for (int i = 0; i < HEAP2_MAX; i++) {
		if (!(heap2bm[i>>3] & 1<<(i&7))) {
			CDR(&heap[HEAP1_MAX + i]) = free2_list;
			free2_list = &heap[HEAP1_MAX + i];
			free2++;
		}
	}
	fprintf(stderr, "POST-GC STATS: free1=%d free2=%d\n", free1, free2);
}

/* Cons */
void *cons_generic(void **free_list, void *car, void *cdr)
{
	void *c = *free_list;
	if (!c) {
		if (free_list == &free1_list) {
			if (car != NUM_TAG) {
				if (car != ATOM_TAG)
					gc_push(&car);
				gc_push(&cdr);
			}
			gc_collect();
			if (car != NUM_TAG) {
				gc_pop();
				if (car != ATOM_TAG)
					gc_pop();
			}
		} else {
			gc_collect();
		}
		c = *free_list;
		if (!c) {
			fprintf(stderr, "OUT OF MEMORY\n");
			ERROR();
		}
	}
	*free_list = CDR(c);
	CAR(c) = car;
	CDR(c) = cdr;
	return c;
}

#define cons(car, cdr) cons_generic(&free1_list, (car), (cdr))
#define cons2() cons_generic(&free2_list, NULL, NULL)

/* Atom */
#define PAIRSIZE ((int)sizeof(struct pair))
void *alloc_string(const char *s)
{
	int slen = strlen(s);
	int len = slen/PAIRSIZE*PAIRSIZE;
	void *ls = NULL;
	gc_push(&ls);
	if (slen % PAIRSIZE)
		ls = cons(strncpy(cons2(), &s[len], PAIRSIZE), NULL);
	while (len != 0)
		ls = cons(memcpy(cons2(), &s[len -= PAIRSIZE], PAIRSIZE), ls);
	gc_pop();
	return ls;
}

int compare_string(const char *s, void *ls)
{
	int len = strlen(s);
	while (len >= PAIRSIZE) {
		if (!ls) // list is shorter
			return 1;
		int res = memcmp(s, CAR(ls), PAIRSIZE);
		if (res)
			return res;
		s += PAIRSIZE;
		len -= PAIRSIZE;
		ls = CDR(ls);
	}
	if (!len) {
		if (ls) // list is longer
			return -1;
		else
			return 0;
	}
	if (!ls) // list is shorter
		return -1;
	int res = strncmp(s, CAR(ls), PAIRSIZE);
	if (res)
		return res;
	if (CDR(ls)) // list is longer
		return -1;
	else
		return 0;
}

void *get(void *c, void *p)
{
	// TODO: move numbers to heap2
	if (NUMBER(c))
		return NULL;
	while (c) {
		if (CAR(c) == p)
			return CADR(c);
		c = CDR(c);
	}
	return NULL;
}
void *get_atom(const char *str)
{
	void *p = atoms_list;
	while (p) {
		void *pname = get(CAR(p), atom_pname);
		if (pname && !compare_string(str, pname))
			return CAR(p) == atom_nil ? NULL : CAR(p);
		p = CDR(p);
	}
	// no such atom, make one
	void *c = cons(ATOM_TAG, cons(atom_pname, cons(alloc_string(str), NULL)));
	atoms_list = cons(c, atoms_list);
	return c;
}

// TODO: add ability to replace stuff
void put_internal(void *atom, void *prop, void *val)
{
	// TODO: gc
	CDR(atom) = cons(val, CDR(atom));
	CDR(atom) = cons(prop, CDR(atom));
}

/* SUBRs */
void *subr_car(void *args, void *a)
{
	(void)a;
	return CAR(args) ? CAAR(args) : ATOM_TAG;
}
void *subr_cdr(void *args, void *a)
{
	(void)a;
	return CAR(args) ? CDAR(args) : CDR(atom_nil);
}
void *subr_cons(void *args, void *a)
{
	(void)a;
	return cons(CAR(args), CADR(args));
}
void *subr_atom(void *args, void *a)
{
	(void)a;
	return !CAR(args) || ATOM(CAR(args)) || NUMBER(CAR(args)) ? atom_t : NULL;
}
void *subr_eq(void *args, void *a)
{
	(void)a;
	return CAR(args) == CADR(args) ? atom_t : NULL;
}
void *equal(void *x, void *y)
{
	if (!x || ATOM(x)) {
		return (!y || ATOM(y)) && x == y ? atom_t : NULL;
	} else if (NUMBER(x)) {
		return NUMBER(y) && CDR(x) == CDR(y) ? atom_t : NULL;
	} else if (!y || ATOM(y) || NUMBER(y)) {
		return NULL;
	} else if (equal(CAR(x), CAR(y))) {
		return equal(CDR(x), CDR(y));
	} else {
		return NULL;
	}
}
void *subr_equal(void *args, void *a)
{
	(void)a;
	return equal(CAR(args), CADR(args));
}
void *subr_list(void *args, void *a)
{
	(void)a;
	return args;
}
void *subr_null(void *args, void *a)
{
	(void)a;
	return !CAR(args) ? atom_t : NULL;
}
void *subr_rplaca(void *args, void *a)
{
	(void)a;
	CAAR(args) = CADR(args);
	return CAR(args);
}
void *subr_rplacd(void *args, void *a)
{
	(void)a;
	CDAR(args) = CADR(args);
	return CAR(args);
}
void *eval(void *fn, void *a, void *unused);
#define eval(a,b) eval(a,b,0)
void *subr_and(void *args, void *a)
{
	void *m = args;
	while (m) {
		// TODO: gc
		void *cond = eval(CAR(m), a);
		if (!cond)
			return NULL;
		m = CDR(m);
	}
	return atom_t;
}
void *subr_or(void *args, void *a)
{
	void *m = args;
	while (m) {
		// TODO: gc
		void *cond = eval(CAR(m), a);
		if (cond)
			return atom_t;
		m = CDR(m);
	}
	return NULL;
}
void *subr_begin(void *args, void *a)
{
	void *m = args;
	void *res = NULL;
	while (m) {
		// TODO: gc
		res = eval(CAR(m), a);
		m = CDR(m);
	}
	return res;
}
void *subr_apply(void *args, void *a)
{
	void *apply(void *fn, void *args, void *a);
	return apply(CAR(args), CADR(args), a);
}
void *subr_eval(void *args, void *a)
{
	(void)a;
	return eval(CAR(args), CADR(args));
}
void *subr_evlis(void *args, void *a)
{
	(void)a;
	void *evlis(void *form, void *a, void *save);
	return evlis(CAR(args), CADR(args), NULL);
}
void *sassoc(void *x, void *y/*, void *u*/);
void *subr_set(void *args, void *a)
{
	if (!ATOM(CAR(args)))
		ERROR();
	return CDR(sassoc(CAR(args), a/*, A5*/)) = CADR(args);
}
void *subr_setq(void *args, void *a)
{
	if (!ATOM(CAR(args)))
		ERROR();
	// TODO: gc
	return CDR(sassoc(CAR(args), a/*, A4*/)) = eval(CADR(args), a);
}
void *subr_define(void *args, void *a)
{
	(void)a;
	void *m = CAR(args);
	while (m) {
		if (!ATOM(CAAR(m)))
			ERROR();
		// TODO: gc
		put_internal(CAAR(m), atom_expr, CADAR(m));
		m = CDR(m);
	}
	return CAR(args); // FIXME: this should return list of atoms?
}
void *subr_deflist(void *args, void *a)
{
	(void)a;
	void *ind = CADR(args);
	if (!ATOM(ind))
		ERROR();
	void *m = CAR(args);
	while (m) {
		if (!ATOM(CAAR(m)))
			ERROR();
		// TODO: gc
		put_internal(CAAR(m), ind, CADAR(m));
		m = CDR(m);
	}
	return CAR(args); // FIXME: this should return list of atoms?
}
void *subr_attrib(void *args, void *a)
{
	(void)a;
	void *c = CAR(args);
	while (CDR(c))
		c = CDR(c);
	return CDR(c) = CADR(args);
}
void *subr_prop(void *args, void *a)
{
	(void)a;
	void *x = CAR(args);
	void *y = CADR(args);
	while (x) {
		if (CAR(x) == y)
			return CDR(x);
		x = CDR(x);
	}
	return eval(CDDR(args), a);
}
void *subr_get(void *args, void *a)
{
	(void)a;
	return get(CAR(args), CADR(args));
}
void *subr_cset(void *args, void *a)
{
	(void)a;
	if (!ATOM(CAR(args)))
		ERROR();
	// TODO: gc
	put_internal(CAR(args), atom_apval, CADR(args));
	return CADR(args);
}
void *subr_csetq(void *args, void *a)
{
	if (!ATOM(CAR(args)))
		ERROR();
	// TODO: gc
	void *val = eval(CADR(args), a);
	// TODO: gc
	put_internal(CAR(args), atom_apval, val);
	return val;
}
void *subr_plus(void *args, void *a)
{
	(void)a;
	intptr_t res = 0;
	while (args) {
		res += (intptr_t)CDAR(args);
		args = CDR(args);
	}
	return cons(NUM_TAG, (void*)res);
}
void *subr_minus(void *args, void *a)
{
	(void)a;
	return cons(NUM_TAG, (void*)-(intptr_t)CDAR(args));
}
void *subr_difference(void *args, void *a)
{
	(void)a;
	return cons(NUM_TAG, (void*)((intptr_t)CDAR(args) - (intptr_t)CDADR(args)));
}
void *subr_times(void *args, void *a)
{
	(void)a;
	intptr_t res = 1;
	while (args) {
		res *= (intptr_t)CDAR(args);
		args = CDR(args);
	}
	return cons(NUM_TAG, (void*)res);
}
void *subr_quotient(void *args, void *a)
{
	(void)a;
	return cons(NUM_TAG, (void*)((intptr_t)CDAR(args) / (intptr_t)CDADR(args)));
}
void *subr_remainder(void *args, void *a)
{
	(void)a;
	return cons(NUM_TAG, (void*)((intptr_t)CDAR(args) % (intptr_t)CDADR(args)));
}
void *subr_divide(void *args, void *a)
{
	(void)a;
	void *rem = (void*)((intptr_t)CDAR(args) / (intptr_t)CDADR(args));
	void *div = (void*)((intptr_t)CDAR(args) % (intptr_t)CDADR(args));
	return cons(div, cons(rem, NULL));
}
void *subr_add1(void *args, void *a)
{
	(void)a;
	return cons(NUM_TAG, (void*)((intptr_t)CDAR(args)+1));
}
void *subr_sub1(void *args, void *a)
{
	(void)a;
	return cons(NUM_TAG, (void*)((intptr_t)CDAR(args)-1));
}
void *subr_min(void *args, void *a)
{
	(void)a;
	intptr_t res = INTPTR_MAX;
	while (args) {
		intptr_t v = (intptr_t)CDAR(args);
		if (v < res)
			res = v;
		args = CDR(args);
	}
	return cons(NUM_TAG, (void*)res);
}
void *subr_max(void *args, void *a)
{
	(void)a;
	intptr_t res = INTPTR_MIN;
	while (args) {
		intptr_t v = (intptr_t)CDAR(args);
		if (res > v)
			res = v;
		args = CDR(args);
	}
	return cons(NUM_TAG, (void*)res);
}
void *subr_recip(void *args, void *a)
{
	(void)args;
	(void)a;
	return cons(NUM_TAG, (void*)0);
}
void *subr_expt(void *args, void *a)
{
	(void)a;
	intptr_t total = 1;
	intptr_t lhs = (intptr_t)CDAR(args);
	intptr_t rhs = (intptr_t)CDADR(args);
	if (rhs < 0)
		return cons(NUM_TAG, (void*)0);
	while (rhs) {
		if (rhs & 1)
			total *= lhs;
		lhs *= lhs;
		rhs >>= 1;
	}
	return cons(NUM_TAG, (void*)total);
}
void *subr_lessp(void *args, void *a)
{
	(void)a;
	return (intptr_t)CDAR(args) < (intptr_t)CDADR(args) ? atom_t : NULL;
}
void *subr_greaterp(void *args, void *a)
{
	(void)a;
	return (intptr_t)CDAR(args) > (intptr_t)CDADR(args) ? atom_t : NULL;
}
void *subr_zerop(void *args, void *a)
{
	(void)a;
	return (intptr_t)CDAR(args) == 0 ? atom_t : NULL;
}
void *subr_onep(void *args, void *a)
{
	(void)a;
	return (intptr_t)CDAR(args) == 1 ? atom_t : NULL;
}
void *subr_minusp(void *args, void *a)
{
	(void)a;
	return (intptr_t)CDAR(args) < 0 ? atom_t : NULL;
}
void *subr_numberp(void *args, void *a)
{
	(void)a;
	return NUMBER(CAR(args)) ? atom_t : NULL;
}
void *subr_logor(void *args, void *a)
{
	(void)a;
	intptr_t res = 0;
	while (args) {
		res |= (intptr_t)CDAR(args);
		args = CDR(args);
	}
	return cons(NUM_TAG, (void*)res);
}
void *subr_logand(void *args, void *a)
{
	(void)a;
	intptr_t res = ~(intptr_t)0;
	while (args) {
		res &= (intptr_t)CDAR(args);
		args = CDR(args);
	}
	return cons(NUM_TAG, (void*)res);
}
void *subr_logxor(void *args, void *a)
{
	(void)a;
	intptr_t res = 0;
	while (args) {
		res ^= (intptr_t)CDAR(args);
		args = CDR(args);
	}
	return cons(NUM_TAG, (void*)res);
}
void *subr_leftshift(void *args, void *a)
{
	(void)a;
	intptr_t lhs = (intptr_t)CDAR(args);
	intptr_t rhs = (intptr_t)CDADR(args);
	if (rhs < 0)
		return cons(NUM_TAG, (void*)(lhs>>-rhs));
	else
		return cons(NUM_TAG, (void*)(lhs<<rhs));
}
void *subr_read(void *args, void *a)
{
	(void)args;
	(void)a;
	int read_(void **slot);
	void *slot;
	gc_push(&slot);
	read_(&slot);
	gc_pop();
	return slot;
}
void *subr_print(void *args, void *a)
{
	(void)a;
	void print(void *c);
	print(CAR(args));
	printf("\n");
	return NULL;
}
void *subr_reclaim(void *args, void *a)
{
	(void)args;
	(void)a;
	gc_collect();
	return NULL;
}
void *subr_error(void *args, void *a)
{
	(void)a;
	void print(void *c);
	printf("ERROR: ");
	print(args);
	printf("\n");
	ERROR();
	return NULL;
}
void *subr_trace(void *args, void *a)
{
	(void)a;
	// FIXME: gc
	for (void *m = CAR(args); m; m = CDR(m))
		put_internal(CAR(m), atom_trace, atom_t);
	return NULL;
}
void *subr_untrace(void *args, void *a)
{
	(void)a;
	// FIXME: gc
	// FIXME: use remprop
	for (void *m = CAR(args); m; m = CDR(m))
		put_internal(CAR(m), atom_trace, NULL);
	return NULL;
}

void init_env()
{
	gc_collect(); // this initializes the heap
	gc_push(&atoms_list);
	gc_push(&evlis_list);

	{
		void *c = cons(NULL, cons(alloc_string("PNAME"), NULL));
		c = atom_pname = CAR(c) = cons(ATOM_TAG, c);
		atoms_list = cons(c, atoms_list);
	}

	atom_expr = get_atom("EXPR");
	atom_fexpr = get_atom("FEXPR");
	atom_subr = get_atom("SUBR");
	atom_fsubr = get_atom("FSUBR");
	atom_apval = get_atom("APVAL");

	atom_label = get_atom("LABEL");
	atom_lambda = get_atom("LAMBDA");
	atom_funarg = get_atom("FUNARG");
	atom_quote = get_atom("QUOTE");
	atom_function = get_atom("FUNCTION");
	atom_cond = get_atom("COND");
	atom_prog = get_atom("PROG");

	atom_nil = get_atom("NIL");
	put_internal(atom_nil, atom_apval, NULL);
	put_internal(get_atom("F"), atom_apval, NULL);
	atom__t_ = get_atom("*T*");
	put_internal(atom__t_, atom_apval, atom__t_);
	atom_t = get_atom("T");
	put_internal(atom_t, atom_apval, atom__t_);

	atom_trace = get_atom("TRACE");

#define DECL_SUBR(atom, s) put_internal(get_atom(atom), atom_subr, cons(NUM_TAG, s))
#define DECL_FSUBR(atom, s) put_internal(get_atom(atom), atom_fsubr, cons(NUM_TAG, s))
	DECL_SUBR("CAR", subr_car);
	DECL_SUBR("CDR", subr_cdr);
	DECL_SUBR("CONS", subr_cons);
	DECL_SUBR("ATOM", subr_atom);
	DECL_SUBR("EQ", subr_eq);
	DECL_SUBR("EQUAL", subr_equal);
	DECL_SUBR("LIST", subr_list);
	DECL_SUBR("NULL", subr_null);
	DECL_SUBR("RPLACA", subr_rplaca);
	DECL_SUBR("RPLACD", subr_rplacd);
	DECL_FSUBR("AND", subr_and);
	DECL_FSUBR("OR", subr_or);
	DECL_SUBR("NOT", subr_null); // intentional
	DECL_FSUBR("BEGIN", subr_begin); // custom
	DECL_SUBR("APPLY", subr_apply);
	DECL_SUBR("EVAL", subr_eval);
	DECL_SUBR("EVLIS", subr_evlis);
	// QUOTE LABEL FUNCTION PROG GO RETURN
	DECL_SUBR("SET", subr_set);
	DECL_FSUBR("SETQ", subr_setq);
	DECL_SUBR("DEFINE", subr_define);
	DECL_SUBR("DEFLIST", subr_deflist);
	DECL_SUBR("ATTRIB", subr_attrib);
	DECL_SUBR("PROP", subr_prop);
	DECL_SUBR("GET", subr_get);
	DECL_SUBR("CSET", subr_cset);
	DECL_FSUBR("CSETQ", subr_csetq);
	// REMPROP FLAG REMFLAG
	// PAIR SASSOC SUBST SUBLIS
	// APPEND CONC NCONC COPY REVERSE MEMBER LENGTH EFFACE (implemented in defun.lisp)
	// MAPLIST MAPCON MAP SEARCH (implemented in defun.lisp)
	DECL_SUBR("PLUS", subr_plus);
	DECL_SUBR("MINUS", subr_minus);
	DECL_SUBR("DIFFERENCE", subr_difference);
	DECL_SUBR("TIMES", subr_times);
	DECL_SUBR("DIVIDE", subr_divide);
	DECL_SUBR("QUOTIENT", subr_quotient);
	DECL_SUBR("REMAINDER", subr_remainder);
	DECL_SUBR("ADD1", subr_add1);
	DECL_SUBR("SUB1", subr_sub1);
	DECL_SUBR("MIN", subr_min);
	DECL_SUBR("MAX", subr_max);
	DECL_SUBR("RECIP", subr_recip);
	DECL_SUBR("EXPT", subr_expt);
	DECL_SUBR("LESSP", subr_lessp);
	DECL_SUBR("GREATERP", subr_greaterp);
	DECL_SUBR("ZEROP", subr_zerop);
	DECL_SUBR("ONEP", subr_onep);
	DECL_SUBR("MINUSP", subr_minusp);
	DECL_SUBR("NUMBERP", subr_numberp);
	DECL_SUBR("FIXP", subr_numberp); // intentional
	DECL_SUBR("FLOATP", subr_recip); // intentional
	DECL_SUBR("LOGOR", subr_logor);
	DECL_SUBR("LOGAND", subr_logand);
	DECL_SUBR("LOGXOR", subr_logxor);
	DECL_SUBR("LEFTSHIFT", subr_leftshift);
	DECL_SUBR("+", subr_plus);
	DECL_SUBR("*", subr_times);
	DECL_SUBR("READ", subr_read);
	DECL_SUBR("PRINT", subr_print);
	DECL_SUBR("RECLAIM", subr_reclaim);
	DECL_SUBR("ERROR", subr_error); // custom-ish
	DECL_SUBR("TRACE", subr_trace); // custom-ish
	DECL_SUBR("UNTRACE", subr_untrace); // custom-ish
}

FILE *current_input = 0;
#ifdef HAVE_LIBEDIT
EditLine *editline = 0;
History *hist = 0;
const char *prompt(EditLine *el)
{
	(void)el;
	return "lisp> ";
}
const char *elbuf_start = 0, *elbuf = 0;
#endif
/* read/print */
int mygetc()
{
#ifdef HAVE_LIBEDIT
	if (current_input == stdin) {
		if (elbuf < elbuf_start) {
			elbuf++;
			return '\n';
		}
		if (!elbuf || !*elbuf) {
			int count;
			elbuf_start = elbuf = el_gets(editline, &count);
			if (!elbuf)
				return EOF;
			if (count > 0) {
				HistEvent ev;
				history(hist, &ev, H_ENTER, elbuf);
			}
		}
		return *elbuf++;
	}
#endif
	return getc(current_input);
}
int myungetc(int c)
{
#ifdef HAVE_LIBEDIT
	if (current_input == stdin) {
		elbuf--;
		return c;
	}
#endif
	return ungetc(c, current_input);
}
int getchar_nows()
{
	int c;
	do {
		c = mygetc();
	} while (c == ' ' || c == '\t' || c == '\n');
	return c;
}

// slot is reachable from gc_roots
int read_(void **slot)
{
	int c = getchar_nows();
	if (c == EOF)
		return -1;
	if (c == ')' || c == '.') {
		fprintf(stderr, "UNEXPECTED '%c'\n", c);
		return 0;
	}
	if (c == '\'') {
		*slot = cons(atom_quote, NULL);
		CDR(*slot) = cons(NULL, NULL);
		return read_(&CADR(*slot));
	}
	if (c == ';') {
		do {
			c = mygetc();
		} while (c != EOF && c != '\n');
		return read_(slot);
	}
	if (c == '#') {
		c = mygetc();
		if (c == ';') {
			int r = read_(slot);
			if (r != 1)
				return r;
			return read_(slot);
		}
		myungetc(c);
		c = '#';
	}
	if (c == '(') {
		c = getchar_nows();
		if (c == ')') {
			*slot = NULL;
			return 1;
		}
		if (c == '.') {
			fprintf(stderr, "UNEXPECTED '%c'\n", c);
			return 0;
		}
		// read car
		void *ls = cons(NULL, NULL);
		*slot = ls;
		myungetc(c);
		read_(&CAR(ls));
		// read rest of the list
		for (;;) {
			c = getchar_nows();
			if (c == EOF)
				return -1;
			if (c == ';') {
				do {
					c = mygetc();
				} while (c != EOF && c != '\n');
				continue;
			}
			if (c == '#') {
				c = mygetc();
				if (c == ';') {
					int r = read_(&CDR(*slot));
					CDR(*slot) = NULL;
					if (r != 1)
						return r;
				}
				myungetc(c); // FIXME: too much ungetc
				c = '#';
			}
			if (c == ')') {
				*slot = ls;
				return 1;
			}
			if (c == '.') {
				int r = read_(&CDR(*slot));
				if (r != 1)
					return r;
				*slot = ls;
				c = getchar_nows();
				if (c == EOF)
					return -1;
				if (c != ')') {
					fprintf(stderr, "UNEXPECTED '%c'\n", c);
					return 0;
				}
				return 1;
			}
			*slot = CDR(*slot) = cons(NULL, NULL);
			myungetc(c);
			read_(&CAR(*slot));
		}
	}
	char buf[32]; // FIXME: make dynamic
	char *p = buf;
	do {
		if (p == (&buf)[1]) {
			fprintf(stderr, "atom too long: '%.32s\n", buf);
			ERROR();
		}
		*p++ = c;
		c = mygetc();
	} while (c != ' ' && c != '\t' && c != '\n' && c != '(' && c != ')' && c != '.' && c != ';' && c != EOF);
	*p++ = '\0';
	myungetc(c);
	if (buf[0] >= '0' && buf[0] <= '9')
		*slot = cons(NUM_TAG, (void*)strtoll(buf, NULL, 0));
	else
		*slot = get_atom(buf);
	return 1;
}

void print(void *c)
{
	if (!c) {
		printf("NIL");
	} else if (CAR(c) == NUM_TAG) {
		printf("%" PRIdPTR, (intptr_t)CDR(c));
	} else if (CAR(c) == ATOM_TAG) {
		void *ls = get(c, atom_pname);
		if (ls) {
				while (ls) {
					printf("%.*s", PAIRSIZE, (char*)CAR(ls));
					ls = CDR(ls);
				}
				return;
		} else {
			printf("!%p!", c);
		}
	} else {
		printf("(");
		print(CAR(c));
		c = CDR(c);
		while (c) {
			if (CAR(c) == ATOM_TAG || CAR(c) == NUM_TAG) {
				printf(" . ");
				print(c);
				break;
			} else {
				printf(" ");
				print(CAR(c));
				c = CDR(c);
			}
		}
		printf(")");
	}
}
/* eval/apply */
void *sassoc(void *x, void *y/*, void *u*/)
{
	while (y) {
		if (CAAR(y) == x)
			return CAR(y);
		y = CDR(y);
	}
	fprintf(stderr, "sassoc failed\n");
	if (ATOM(x)) {
		printf("atom in question: ");
		print(x);
		printf("\n");
	}
	ERROR();
}
void *pair(void *x, void *y)
{
	void *m = NULL;
	gc_push(&m);
	gc_push(&x);
	gc_push(&y);
	while (x && y) {
		m = cons(cons(CAR(x), CAR(y)), m);
		x = CDR(x);
		y = CDR(y);
	}
	gc_pop();
	gc_pop();
	gc_pop();
	if (x || y) {
		/* F2 F3 */
		fprintf(stderr, "pair error\n");
		ERROR();
	}
	return m;
}
void *nconc(void *x, void *y)
{
	if (!x)
		return y;
	void *m = x;
	while (CDR(m))
		m = CDR(m);
	CDR(m) = y;
	return x;
}
void *nconc_smart(void *x, void *y)
{
	void *xx = x;
	while (xx && y && CAAR(xx) == CAAR(y)) {
		xx = CDR(xx);
		y = CDR(y);
	}
	return nconc(x, y);
}
#define INVOKE(subr, args, a) (((void*(*)(void*, void*))CDR(subr))(args, a))
void *apply(void *fn, void *args, void *a)
{
	if (!fn)
		return NULL;
	if (ATOM(fn) || NUMBER(fn)) {
		void *expr = get(fn, atom_expr);
		if (expr)
			TAILCALL apply(expr, args, a);
		void *subr = get(fn, atom_subr);
		if (subr)
			return INVOKE(subr, args, a);
		TAILCALL apply(CDR(sassoc(fn, a/*, A2*/)), args, a);
	}
	if (CAR(fn) == atom_label) {
		gc_push(&args);
		gc_push(&a);
		a = cons(cons(CADR(fn), CADDR(fn)), a);
		gc_pop();
		gc_pop();
		TAILCALL apply(CDAR(a), args, a);
	}
	if (CAR(fn) == atom_funarg) {
		TAILCALL apply(CADR(fn), args, CADDR(fn));
	}
	if (CAR(fn) == atom_lambda) {
		void *body = CADDR(fn);
		gc_push(&body);
		gc_push(&a);
		if (debug) {
			printf("running lambda ");
			print(CADR(fn));
			printf(" ");
			print(args);
			printf("\n");
		}
		a = nconc_smart(pair(CADR(fn), args), a);
		gc_pop();
		gc_pop();
		TAILCALL eval(body, a);
	}
	TAILCALL apply(eval(fn, a), args, a);
}
void *evcon(void *c, void *a, void *unused)
{
	(void)unused;
	while (c) {
		// TODO: gc
		void *antecedent = eval(CAAR(c), a);
		if (antecedent)
			return eval(CADAR(c), a);
		c = CDR(c);
	}
	return NULL; // FIXME: a3
}
void *evlis(void *m, void *a, void *save)
{
	if (!m)
		return NULL;

	// push (NIL m a . save) onto the the list
	gc_push(&m);
	evlis_list = cons(cons(NULL, cons(m, cons(a, save))), evlis_list);
	gc_pop();

	void **p = &CAAR(evlis_list);
	void **mp = &CADAR(evlis_list); // small optimization so that evaled stuff can get GC'd
	while (m) {
		*p = cons(eval(CAR(m), a), NULL);
		p = &CDR(*p);
		*mp = m = CDR(m);
	}
	void *ls = CAAR(evlis_list);
	evlis_list = CDR(evlis_list);
	return ls;
}
void *(eval)(void *form, void *a, void *unused)
{
	(void)unused;
	if (!form)
		return NULL;
	if (NUMBER(form))
		return form;
	if (ATOM(form)) {
		void *apval = get(form, atom_apval);
		if (apval)
			return apval;
		else
			return CDR(sassoc(form, a/*, A8*/));
	}
	void *carform = CAR(form);
	void *cdrform = CDR(form);
	if (carform == atom_quote) // TODO: change to FSUBR?
		return CAR(cdrform);
	if (carform == atom_function) // TODO: change to FSUBR?
		return cons(atom_funarg, cons(CAR(cdrform), cons(a, NULL))); // FIXME: gc, TODO: list
	if (carform == atom_cond)
		TAILCALL evcon(cdrform, a, 0);
	//if (carform == atom_prog) // NYI, FSUBR?
	if (ATOM(carform)) {
		void *expr = get(carform, atom_expr);
		if (expr) {
			if (get(carform, atom_trace)) {
				// probably lots of gc issues
				void *args = evlis(cdrform, a, expr);
				printf("ARGUMENTS OF ");
				print(carform);
				for (void *m = args; m; m = CDR(m)) {
					printf("\n");
					print(CAR(m));
				}
				printf("\n\n");
				void *val = apply(expr, args, a);
				printf("VALUE OF ");
				print(carform);
				printf("\n");
				print(val);
				printf("\n\n");
				return val;
			} else {
				TAILCALL apply(expr, evlis(cdrform, a, expr), a);
			}
		}
		void *fexpr = get(carform, atom_fexpr);
		if (fexpr) {
			gc_push(&form);
			void *args = cons(cdrform, cons(a, NULL)); // TODO: list?
			gc_pop();
			TAILCALL apply(fexpr, args, a);
		}
		void *subr = get(carform, atom_subr);
		if (subr)
			return INVOKE(subr, evlis(cdrform, a, subr), a);
		void *fsubr = get(carform, atom_fsubr);
		if (fsubr)
			return INVOKE(fsubr, cdrform, a);
		gc_push(&a);
		form = cons(CDR(sassoc(carform, a/*, A9*/)), cdrform);
		gc_pop();
		TAILCALL eval(form, a);
	}
	TAILCALL apply(carform, evlis(cdrform, a, carform), a);
}
void *evalquote(void *fn, void *args)
{
	if (get(fn, atom_fexpr) || get(fn, atom_fsubr))
		return eval(cons(fn,args), NULL);
	else
		return apply(fn, args, NULL);
}

int main(int argc, char *argv[])
{
	(void)argc;
	init_env();
#ifdef HAVE_LIBEDIT
	editline = el_init("mylisp", stdin, stdout, stderr);
	if (!editline)
		abort();
	el_set(editline, EL_PROMPT, &prompt);
	el_set(editline, EL_EDITOR, "emacs");
	hist = history_init();
	if (!hist)
		abort();
	HistEvent ev;
	history(hist, &ev, H_SETSIZE, 1000);
	el_set(editline, EL_HIST, history, hist);
#endif
	void *volatile repl = NULL;
	gc_push((void **)&repl);
	volatile int save_nroots = gc_nroots;
	char **volatile argp = &argv[1];
	if (setjmp(jbuf)) {
		fprintf(stderr, "something bad happened\n");
		gc_nroots = save_nroots;
		evlis_list = NULL;
	}
	jbuf_inited = 1;
	while (*argp) {
		if (!strcmp(*argp, "-d")) {
			argp++;
			debug = 1;
			continue;
		}
		current_input = fopen(*argp++, "r");
		if (current_input) {
			while (read_((void **)&repl) != -1) {
				eval(repl, NULL);
			}
		}
	}
	current_input = stdin;
	for (;;) {
		int r = read_((void **)&repl);
		if (r == 1) {
			print(eval(repl, NULL));
			printf("\n");
		} else if (r == -1) {
			break;
		}
	}
#ifdef HAVE_LIBEDIT
	history_end(hist);
	el_end(editline);
#endif
	return 0;
}
