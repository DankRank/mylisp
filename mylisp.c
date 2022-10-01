#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
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
#define ATOM(p) (CAR(p) == ATOM_TAG)
#define NUMBER(p) (CAR(p) == NUM_TAG)

#define CAAR(p) CAR(CAR(p))
#define CADR(p) CAR(CDR(p))
#define CDAR(p) CDR(CAR(p))
#define CADDR(p) CAR(CDR(CDR(p)))
#define CADAR(p) CAR(CDR(CAR(p)))
#define CDADR(p) CDR(CAR(CDR(p)))

void *free1_list = NULL;
void *free2_list = NULL;
void *atoms_list = NULL;

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

/* GC */
void **gc_roots[64];
int gc_nroots = 0;
int gc_watermark = 0;
void gc_push(void **p)
{
	if (gc_nroots == 64) {
		fprintf(stderr, "OUT OF GCROOTS\n");
		abort();
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

void *cons_generic(void **free_list, void *car, void *cdr)
{
	void *c = *free_list;
	if (!c) {
		// TODO: GC
		fprintf(stderr, "OUT OF MEMORY\n");
		abort();
	}
	*free_list = CDR(c);
	CAR(c) = car;
	CDR(c) = cdr;
	return c;
}

#define cons(car, cdr) cons_generic(&free1_list, (car), (cdr))
#define cons2() cons_generic(&free2_list, NULL, NULL)

#define PAIRSIZE ((int)sizeof(struct pair))
void *alloc_string(const char *s)
{
	int len = strlen(s)/PAIRSIZE*PAIRSIZE;
	void *c = strncpy(cons2(), &s[len], PAIRSIZE);
	gc_push(&c);
	void *ls = cons(c, NULL);
	gc_push(&ls);
	while (len != 0) {
		c = memcpy(cons2(), &s[len -= PAIRSIZE], PAIRSIZE);
		ls = cons(c, ls);
	}
	gc_pop();
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
			return CAR(p);
		p = CDR(p);
	}
	// no such atom, make one
	void *c = alloc_string(str);
	gc_push(&c);
	c = cons(c, NULL);
	c = cons(atom_pname, c);
	c = cons(ATOM_TAG, c);
	atoms_list = cons(c, atoms_list);
	gc_pop();
	return c;
}

// TODO: add ability to replace stuff
void put_internal(void *atom, void *prop, void *val)
{
	CDR(atom) = cons(val, CDR(atom));
	CDR(atom) = cons(prop, CDR(atom));
}

void *subr_car(void *args, void *a)
{
	(void)a;
	return CAAR(args);
}
void *subr_cdr(void *args, void *a)
{
	(void)a;
	return CDAR(args);
}
void *subr_cons(void *args, void *a)
{
	(void)a;
	return cons(CAR(args), CADR(args));
}
void *subr_atom(void *args, void *a)
{
	(void)a;
	return ATOM(CAR(args)) || NUMBER(CAR(args)) ? atom_t : NULL;
}
void *subr_eq(void *args, void *a)
{
	(void)a;
	return CAR(args) == CADR(args) ? atom_t : NULL;
}
void *equal(void *x, void *y)
{
	if (ATOM(x) || NUMBER(x)) {
		if (ATOM(y) || NUMBER(y))
			return x == y ? atom_t : NULL;
		else
			return NULL;
	} else if (ATOM(y) || NUMBER(y)) {
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
	return CAR(args) == 0 || CAR(args) == atom_nil ? atom_t : NULL;
}
void *subr_rplaca(void *args, void *a)
{
	(void)a;
	return CAAR(args) = CADR(args);
}
void *subr_rplacd(void *args, void *a)
{
	(void)a;
	return CDAR(args) = CADR(args);
}
void *eval(void *fn, void *a);
void *subr_and(void *args, void *a)
{
	void *m = args;
	while (m) {
		void *cond = eval(CAR(m), a);
		if (!cond || cond == atom_nil)
			return NULL;
		m = CDR(m);
	}
	return atom_t;
}
void *subr_or(void *args, void *a)
{
	void *m = args;
	while (m) {
		void *cond = eval(CAR(m), a);
		if (cond && cond != atom_nil)
			return atom_t;
		m = CDR(m);
	}
	return NULL;
}
void *subr_define(void *args, void *a)
{
	(void)a;
	void *m = CAR(args);
	while (m) {
		if (!ATOM(CAAR(m)))
			abort();
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
		abort();
	void *m = CAR(args);
	while (m) {
		if (!ATOM(CAAR(m)))
			abort();
		put_internal(CAAR(m), ind, CADAR(m));
		m = CDR(m);
	}
	return CAR(args); // FIXME: this should return list of atoms?
}
void *subr_cset(void *args, void *a)
{
	(void)a;
	if (!ATOM(CAR(args)))
		abort();
	put_internal(CAR(args), atom_apval, CADR(args));
	return CADR(args);
}
void *subr_csetq(void *args, void *a)
{
	if (!ATOM(CAR(args)))
		abort();
	void *val = eval(CADR(args), a);
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
	// FIXME: gc
	return cons(subr_quotient(args, a), cons(subr_remainder(args, a), NULL));
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
void *subr_reclaim(void *args, void *a)
{
	(void)args;
	(void)a;
	gc_collect();
	return NULL;
}

void init_env()
{
	gc_collect(); // this initializes the heap
	gc_push(&atoms_list);

	{
		void *c = alloc_string("PNAME");
		gc_push(&c);
		c = cons(c, NULL);
		c = cons(NULL, c);
		c = atom_pname = CAR(c) = cons(ATOM_TAG, c);
		atoms_list = cons(c, atoms_list);
		gc_pop();
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
	atom_f = get_atom("F");
	put_internal(atom_nil, atom_apval, NULL);
	atom__t_ = get_atom("*T*");
	put_internal(atom__t_, atom_apval, atom__t_);
	atom_t = get_atom("T");
	put_internal(atom_t, atom_apval, atom__t_);

	// FIXME: it's a GC bug to put these directly into the subr prop
	put_internal(get_atom("CAR"), atom_subr, subr_car);
	put_internal(get_atom("CDR"), atom_subr, subr_cdr);
	put_internal(get_atom("CONS"), atom_subr, subr_cons);
	put_internal(get_atom("ATOM"), atom_subr, subr_atom);
	put_internal(get_atom("EQ"), atom_subr, subr_eq);
	put_internal(get_atom("EQUAL"), atom_subr, subr_equal);
	put_internal(get_atom("LIST"), atom_subr, subr_list);
	put_internal(get_atom("NULL"), atom_subr, subr_null);
	put_internal(get_atom("RPLACA"), atom_subr, subr_rplaca);
	put_internal(get_atom("RPLACD"), atom_subr, subr_rplacd);
	put_internal(get_atom("AND"), atom_fsubr, subr_and);
	put_internal(get_atom("OR"), atom_fsubr, subr_or);
	put_internal(get_atom("NOT"), atom_fsubr, subr_null); // intentional
	// APPLY EVAL EVLIS QUOTE LABEL FUNCTION PROG GO RETURN SET SETQ
	put_internal(get_atom("DEFINE"), atom_subr, subr_define);
	put_internal(get_atom("DEFLIST"), atom_subr, subr_deflist);
	// ATTRIB PROP GET
	put_internal(get_atom("CSET"), atom_subr, subr_cset);
	put_internal(get_atom("CSETQ"), atom_fsubr, subr_csetq);
	// REMPROP FLAG REMFLAG
	// PAIR SASSOC SUBST SUBLIS
	// APPEND CONC NCONC COPY REVERSE MEMBER LENGTH EFFACE
	// MAPLIST MAPCON MAP SEARCH 
	put_internal(get_atom("PLUS"), atom_subr, subr_plus);
	put_internal(get_atom("MINUS"), atom_subr, subr_minus);
	put_internal(get_atom("DIFFERENCE"), atom_subr, subr_difference);
	put_internal(get_atom("TIMES"), atom_subr, subr_times);
	put_internal(get_atom("DIVIDE"), atom_subr, subr_divide);
	put_internal(get_atom("QUOTIENT"), atom_subr, subr_quotient);
	put_internal(get_atom("REMAINDER"), atom_subr, subr_remainder);
	put_internal(get_atom("ADD1"), atom_subr, subr_add1);
	put_internal(get_atom("SUB1"), atom_subr, subr_sub1);
	put_internal(get_atom("MIN"), atom_subr, subr_min);
	put_internal(get_atom("MAX"), atom_subr, subr_max);
	put_internal(get_atom("RECIP"), atom_subr, subr_recip);
	put_internal(get_atom("EXPT"), atom_subr, subr_expt);
	put_internal(get_atom("LESSP"), atom_subr, subr_lessp);
	put_internal(get_atom("GREATERP"), atom_subr, subr_greaterp);
	put_internal(get_atom("ZEROP"), atom_subr, subr_zerop);
	put_internal(get_atom("ONEP"), atom_subr, subr_onep);
	put_internal(get_atom("MINUSP"), atom_subr, subr_minusp);
	put_internal(get_atom("NUMBERP"), atom_subr, subr_numberp);
	put_internal(get_atom("FIXP"), atom_subr, subr_numberp); // intentional
	put_internal(get_atom("FLOATP"), atom_subr, subr_recip); // intentional
	put_internal(get_atom("LOGOR"), atom_subr, subr_logor);
	put_internal(get_atom("LOGAND"), atom_subr, subr_logand);
	put_internal(get_atom("LOGXOR"), atom_subr, subr_logxor);
	put_internal(get_atom("LEFTSHIFT"), atom_subr, subr_leftshift);
	put_internal(get_atom("+"), atom_subr, subr_plus);
	put_internal(get_atom("*"), atom_subr, subr_times);
	// READ PRINT 
	put_internal(get_atom("RECLAIM"), atom_subr, subr_reclaim);
}

int getchar_nows()
{
	int c;
	do {
		c = getchar();
	} while (c == ' ' || c == '\t' || c == '\n');
	return c;
}

// slot is reachable from gc_roots
void read(void **slot)
{
	int c = getchar_nows();
	if (c == ')' || c == '.') {
		fprintf(stderr, "UNEXPECTED '%c'\n", c);
		return;
	}
	if (c == '\'') {
		*slot = cons(atom_quote, NULL);
		CDR(*slot) = cons(NULL, NULL);
		read(&CADR(*slot));
		return;
	}
	if (c == '(') {
		c = getchar_nows();
		if (c == ')') {
			*slot = NULL;
			return;
		}
		if (c == '.') {
			fprintf(stderr, "UNEXPECTED '%c'\n", c);
			return;
		}
		// read car
		void *ls = cons(NULL, NULL);
		*slot = ls;
		ungetc(c, stdin);
		read(&CAR(ls));
		// read rest of the list
		for (;;) {
			c = getchar_nows();
			if (c == ')') {
				*slot = ls;
				return;
			}
			if (c == '.') {
				read(&CDR(*slot));
				*slot = ls;
				c = getchar_nows();
				if (c != ')')
					fprintf(stderr, "UNEXPECTED '%c'\n", c);
				return;
			}
			*slot = CDR(*slot) = cons(NULL, NULL);
			ungetc(c, stdin);
			read(&CAR(*slot));
		}
	}
	if (c == EOF)
		abort(); // FIXME: handle this better
	char buf[32]; // FIXME: make dynamic
	char *p = buf;
	do {
		*p++ = c;
		c = getchar();
	} while (c != ' ' && c != '\t' && c != '\n' && c != '(' && c != ')' && c != '.' && c != EOF);
	*p++ = '\0';
	ungetc(c, stdin);
	if (buf[0] >= '0' && buf[0] <= '9')
		*slot = cons(NUM_TAG, (void*)strtoll(buf, NULL, 0));
	else
		*slot = get_atom(buf);
}

void print(void *c)
{
	if (!c) {
		printf("()");
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
			if (CAR(c) == ATOM_TAG) {
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

void *sassoc(void *x, void *y/*, void *u*/)
{
	while (y) {
		if (CAAR(y) == x)
			return CAR(y);
		y = CDR(y);
	}
	fprintf(stderr, "sassoc failed\n");
	abort();
}
void *pair(void *x, void *y)
{
	void *m = NULL;
	gc_push(&m);
	while (x && y) {
		m = cons(NULL, m);
		CAR(m) = cons(CAR(x), CAR(y));
		x = CDR(x);
		y = CDR(y);
	}
	gc_pop();
	if (x || y) {
		/* F2 F3 */
		fprintf(stderr, "pair error\n");
		abort();
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
void *apply(void *fn, void *args, void *a)
{
	if (!fn)
		return NULL;
	if (ATOM(fn)) {
		void *expr = get(fn, atom_expr);
		if (expr)
			return apply(expr, args, a);
		void *subr = get(fn, atom_subr);
		if (subr)
			return ((void*(*)(void*, void*))subr)(args, a);
		return apply(CDR(sassoc(fn, a/*, A2*/)), args, a);
	}
	if (CAR(fn) == atom_label) {
		// FIXME: gc
		return apply(CADDR(fn), args, cons(cons(CADR(fn), CADDR(fn)), a));
	}
	if (CAR(fn) == atom_funarg) {
		return apply(CADR(fn), args, CADDR(fn));
	}
	if (CAR(fn) == atom_lambda) {
		// FIXME: gc
		return eval(CADDR(fn), nconc(pair(CADR(fn), args), a));
	}
	return apply(eval(fn, a), args, a);
}
void *evcon(void *c, void *a)
{
	while (c) {
		void *antecedent = eval(CAAR(c), a);
		if (antecedent && antecedent != atom_nil)
			return eval(CADAR(c), a);
		c = CDR(c);
	}
	return NULL; // FIXME: a3
}
void *evlis(void *m, void *a)
{
	void *ls = NULL; // FIXME: gc
	void **p = &ls;
	while (m) {
		*p = cons(eval(CAR(m), a), NULL);
		p = &CDR(*p);
		m = CDR(m);
	}
	return ls;
}
void *eval(void *form, void *a)
{
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
	if (carform == atom_quote) // TODO: change to FSUBR?
		return CADR(form);
	if (carform == atom_function) // TODO: change to FSUBR?
		return cons(atom_funarg, cons(CADR(form), cons(a, NULL))); // FIXME: gc, TODO: list
	if (carform == atom_cond)
		return evcon(CDR(form), a);
	//if (carform == atom_prog) // NYI, FSUBR?
	if (ATOM(carform)) {
		void *expr = get(carform, atom_expr);
		if (expr)
			return apply(expr, evlis(CDR(form), a), a);
		void *fexpr = get(carform, atom_fexpr);
		if (fexpr)
			return apply(fexpr, cons(CDR(form), cons(a, NULL)), a); // FIXME: gc, TODO: list
		void *subr = get(carform, atom_subr);
		if (subr)
			return ((void*(*)(void*, void*))subr)(evlis(CDR(form), a), a);
		void *fsubr = get(carform, atom_fsubr);
		if (fsubr)
			return ((void*(*)(void*, void*))fsubr)(CDR(form), a);
		return eval(cons(CDR(sassoc(carform, a/*, A9*/)),CDR(form)), a);
	}
	return apply(carform, evlis(CDR(form), a), a);
}
void *evalquote(void *fn, void *args)
{
	if (get(fn, atom_fexpr) || get(fn, atom_fsubr))
		return eval(cons(fn,args), NULL); // FIXME: gc
	else
		return apply(fn, args, NULL);
}

int main()
{
	init_env();
	void *repl = NULL;
	gc_push(&repl);
	for (;;) {
		read(&repl);
		print(repl);
		printf(" => ");
		print(eval(repl, NULL));
		printf("\n");
	}
	return 0;
}
