# 0 "trap.c"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "trap.c"
# 1 "types.h" 1
typedef unsigned int uint;
typedef unsigned short ushort;
typedef unsigned char uchar;
typedef uint pde_t;
# 2 "trap.c" 2
# 1 "defs.h" 1
struct buf;
struct context;
struct file;
struct inode;
struct pipe;
struct proc;
struct rtcdate;
struct spinlock;
struct sleeplock;
struct stat;
struct superblock;


void binit(void);
struct buf* bread(uint, uint);
void brelse(struct buf*);
void bwrite(struct buf*);


void consoleinit(void);
void cprintf(char*, ...);
void consoleintr(int(*)(void));
void panic(char*) __attribute__((noreturn));


int exec(char*, char**);


struct file* filealloc(void);
void fileclose(struct file*);
struct file* filedup(struct file*);
void fileinit(void);
int fileread(struct file*, char*, int n);
int filestat(struct file*, struct stat*);
int filewrite(struct file*, char*, int n);


void readsb(int dev, struct superblock *sb);
int dirlink(struct inode*, char*, uint);
struct inode* dirlookup(struct inode*, char*, uint*);
struct inode* ialloc(uint, short);
struct inode* idup(struct inode*);
void iinit(int dev);
void ilock(struct inode*);
void iput(struct inode*);
void iunlock(struct inode*);
void iunlockput(struct inode*);
void iupdate(struct inode*);
int namecmp(const char*, const char*);
struct inode* namei(char*);
struct inode* nameiparent(char*, char*);
int readi(struct inode*, char*, uint, uint);
void stati(struct inode*, struct stat*);
int writei(struct inode*, char*, uint, uint);


void ideinit(void);
void ideintr(void);
void iderw(struct buf*);


void ioapicenable(int irq, int cpu);
extern uchar ioapicid;
void ioapicinit(void);


char* kalloc(void);
void kfree(char*);
void kinit1(void*, void*);
void kinit2(void*, void*);


void kbdintr(void);


void cmostime(struct rtcdate *r);
int lapicid(void);
extern volatile uint* lapic;
void lapiceoi(void);
void lapicinit(void);
void lapicstartap(uchar, uint);
void microdelay(int);


void initlog(int dev);
void log_write(struct buf*);
void begin_op();
void end_op();


extern int ismp;
void mpinit(void);


void picenable(int);
void picinit(void);


int pipealloc(struct file**, struct file**);
void pipeclose(struct pipe*, int);
int piperead(struct pipe*, char*, int);
int pipewrite(struct pipe*, char*, int);



int cpuid(void);
void exit(void);
int fork(void);
int growproc(int);
int kill(int);
struct cpu* mycpu(void);
struct proc* myproc();
void pinit(void);
void procdump(void);
void scheduler(void) __attribute__((noreturn));
void sched(void);
void setproc(struct proc*);
void sleep(void*, struct spinlock*);
void userinit(void);
int wait(void);
void wakeup(void*);
void yield(void);


void swtch(struct context**, struct context*);


void acquire(struct spinlock*);
void getcallerpcs(void*, uint*);
int holding(struct spinlock*);
void initlock(struct spinlock*, char*);
void release(struct spinlock*);
void pushcli(void);
void popcli(void);


void acquiresleep(struct sleeplock*);
void releasesleep(struct sleeplock*);
int holdingsleep(struct sleeplock*);
void initsleeplock(struct sleeplock*, char*);


int memcmp(const void*, const void*, uint);
void* memmove(void*, const void*, uint);
void* memset(void*, int, uint);
char* safestrcpy(char*, const char*, int);
int strlen(const char*);
int strncmp(const char*, const char*, uint);
char* strncpy(char*, const char*, int);


int argint(int, int*);
int argptr(int, char**, int);
int argstr(int, char**);
int fetchint(uint, int*);
int fetchstr(uint, char**);
void syscall(void);


void timerinit(void);


void idtinit(void);
extern uint ticks;
void tvinit(void);
extern struct spinlock tickslock;


void uartinit(void);
void uartintr(void);
void uartputc(int);


void seginit(void);
void kvmalloc(void);
pde_t* setupkvm(void);
char* uva2ka(pde_t*, char*);
int allocuvm(pde_t*, uint, uint);
int deallocuvm(pde_t*, uint, uint);
void freevm(pde_t*);
void inituvm(pde_t*, char*, uint);
int loaduvm(pde_t*, char*, struct inode*, uint, uint);
pde_t* copyuvm(pde_t*, uint);
void switchuvm(struct proc*);
void switchkvm(void);
int copyout(pde_t*, uint, void*, uint);
void clearpteu(pde_t *pgdir, char *uva);
# 3 "trap.c" 2
# 1 "param.h" 1
# 4 "trap.c" 2
# 1 "memlayout.h" 1
# 5 "trap.c" 2
# 1 "mmu.h" 1
# 26 "mmu.h"
struct segdesc {
  uint lim_15_0 : 16;
  uint base_15_0 : 16;
  uint base_23_16 : 8;
  uint type : 4;
  uint s : 1;
  uint dpl : 2;
  uint p : 1;
  uint lim_19_16 : 4;
  uint avl : 1;
  uint rsv1 : 1;
  uint db : 1;
  uint g : 1;
  uint base_31_24 : 8;
};
# 104 "mmu.h"
typedef uint pte_t;


struct taskstate {
  uint link;
  uint esp0;
  ushort ss0;
  ushort padding1;
  uint *esp1;
  ushort ss1;
  ushort padding2;
  uint *esp2;
  ushort ss2;
  ushort padding3;
  void *cr3;
  uint *eip;
  uint eflags;
  uint eax;
  uint ecx;
  uint edx;
  uint ebx;
  uint *esp;
  uint *ebp;
  uint esi;
  uint edi;
  ushort es;
  ushort padding4;
  ushort cs;
  ushort padding5;
  ushort ss;
  ushort padding6;
  ushort ds;
  ushort padding7;
  ushort fs;
  ushort padding8;
  ushort gs;
  ushort padding9;
  ushort ldt;
  ushort padding10;
  ushort t;
  ushort iomb;
};


struct gatedesc {
  uint off_15_0 : 16;
  uint cs : 16;
  uint args : 5;
  uint rsv1 : 3;
  uint type : 4;
  uint s : 1;
  uint dpl : 2;
  uint p : 1;
  uint off_31_16 : 16;
};
# 6 "trap.c" 2
# 1 "proc.h" 1

struct cpu {
  uchar apicid;
  struct context *scheduler;
  struct taskstate ts;
  struct segdesc gdt[6];
  volatile uint started;
  int ncli;
  int intena;
  struct proc *proc;
};

extern struct cpu cpus[8];
extern int ncpu;
# 27 "proc.h"
struct context {
  uint edi;
  uint esi;
  uint ebx;
  uint ebp;
  uint eip;
};

enum procstate { UNUSED, EMBRYO, SLEEPING, RUNNABLE, RUNNING, ZOMBIE };


struct proc {
  uint sz;
  pde_t* pgdir;
  char *kstack;
  enum procstate state;
  int pid;
  struct proc *parent;
  struct trapframe *tf;
  struct context *context;
  void *chan;
  int killed;
  struct file *ofile[16];
  struct inode *cwd;
  char name[16];
};
# 7 "trap.c" 2
# 1 "x86.h" 1


static inline uchar
inb(ushort port)
{
  uchar data;

  asm volatile("in %1,%0" : "=a" (data) : "d" (port));
  return data;
}

static inline void
insl(int port, void *addr, int cnt)
{
  asm volatile("cld; rep insl" :
               "=D" (addr), "=c" (cnt) :
               "d" (port), "0" (addr), "1" (cnt) :
               "memory", "cc");
}

static inline void
outb(ushort port, uchar data)
{
  asm volatile("out %0,%1" : : "a" (data), "d" (port));
}

static inline void
outw(ushort port, ushort data)
{
  asm volatile("out %0,%1" : : "a" (data), "d" (port));
}

static inline void
outsl(int port, const void *addr, int cnt)
{
  asm volatile("cld; rep outsl" :
               "=S" (addr), "=c" (cnt) :
               "d" (port), "0" (addr), "1" (cnt) :
               "cc");
}

static inline void
stosb(void *addr, int data, int cnt)
{
  asm volatile("cld; rep stosb" :
               "=D" (addr), "=c" (cnt) :
               "0" (addr), "1" (cnt), "a" (data) :
               "memory", "cc");
}

static inline void
stosl(void *addr, int data, int cnt)
{
  asm volatile("cld; rep stosl" :
               "=D" (addr), "=c" (cnt) :
               "0" (addr), "1" (cnt), "a" (data) :
               "memory", "cc");
}

struct segdesc;

static inline void
lgdt(struct segdesc *p, int size)
{
  volatile ushort pd[3];

  pd[0] = size-1;
  pd[1] = (uint)p;
  pd[2] = (uint)p >> 16;

  asm volatile("lgdt (%0)" : : "r" (pd));
}

struct gatedesc;

static inline void
lidt(struct gatedesc *p, int size)
{
  volatile ushort pd[3];

  pd[0] = size-1;
  pd[1] = (uint)p;
  pd[2] = (uint)p >> 16;

  asm volatile("lidt (%0)" : : "r" (pd));
}

static inline void
ltr(ushort sel)
{
  asm volatile("ltr %0" : : "r" (sel));
}

static inline uint
readeflags(void)
{
  uint eflags;
  asm volatile("pushfl; popl %0" : "=r" (eflags));
  return eflags;
}

static inline void
loadgs(ushort v)
{
  asm volatile("movw %0, %%gs" : : "r" (v));
}

static inline void
cli(void)
{
  asm volatile("cli");
}

static inline void
sti(void)
{
  asm volatile("sti");
}

static inline uint
xchg(volatile uint *addr, uint newval)
{
  uint result;


  asm volatile("lock; xchgl %0, %1" :
               "+m" (*addr), "=a" (result) :
               "1" (newval) :
               "cc");
  return result;
}

static inline uint
rcr2(void)
{
  uint val;
  asm volatile("movl %%cr2,%0" : "=r" (val));
  return val;
}

static inline void
lcr3(uint val)
{
  asm volatile("movl %0,%%cr3" : : "r" (val));
}




struct trapframe {

  uint edi;
  uint esi;
  uint ebp;
  uint oesp;
  uint ebx;
  uint edx;
  uint ecx;
  uint eax;


  ushort gs;
  ushort padding1;
  ushort fs;
  ushort padding2;
  ushort es;
  ushort padding3;
  ushort ds;
  ushort padding4;
  uint trapno;


  uint err;
  uint eip;
  ushort cs;
  ushort padding5;
  uint eflags;


  uint esp;
  ushort ss;
  ushort padding6;
};
# 8 "trap.c" 2
# 1 "traps.h" 1
# 9 "trap.c" 2
# 1 "spinlock.h" 1

struct spinlock {
  uint locked;


  char *name;
  struct cpu *cpu;
  uint pcs[10];

};
# 10 "trap.c" 2


struct gatedesc idt[256];
extern uint vectors[];
struct spinlock tickslock;
uint ticks;

void
tvinit(void)
{
  int i;

  for(i = 0; i < 256; i++)
  { 
    (idt[i]).off_15_0 = (uint)(vectors[i]) & 0xffff;
    (idt[i]).cs = (1<<3);
    (idt[i]).args = 0;
    (idt[i]).rsv1 = 0;
    (idt[i]).type = (0) ? 0xF : 0xE;
    (idt[i]).s = 0;
    (idt[i]).dpl = (0);
    (idt[i]).p = 1;
    (idt[i]).off_31_16 = (uint)(vectors[i]) >> 16;
 };

  { (idt[64]).off_15_0 = (uint)(vectors[64]) & 0xffff; (idt[64]).cs = (1<<3); (idt[64]).args = 0; (idt[64]).rsv1 = 0; (idt[64]).type = (1) ? 0xF : 0xE; (idt[64]).s = 0; (idt[64]).dpl = (0x3); (idt[64]).p = 1; (idt[64]).off_31_16 = (uint)(vectors[64]) >> 16; };

  initlock(&tickslock, "time");
}

void
idtinit(void)
{
  lidt(idt, sizeof(idt));
}


void
trap(struct trapframe *tf)
{
  if(tf->trapno == 64){
    if(myproc()->killed)
      exit();
    myproc()->tf = tf;
    syscall();
    if(myproc()->killed)
      exit();
    return;
  }

  switch(tf->trapno){
  case 32 + 0:
    if(cpuid() == 0){
      acquire(&tickslock);
      ticks++;
      wakeup(&ticks);
      release(&tickslock);
    }
    lapiceoi();
    break;
  case 32 + 14:
    ideintr();
    lapiceoi();
    break;
  case 32 + 14 +1:

    break;
  case 32 + 1:
    kbdintr();
    lapiceoi();
    break;
  case 32 + 4:
    uartintr();
    lapiceoi();
    break;
  case 32 + 7:
  case 32 + 31:
    cprintf("cpu%d: spurious interrupt at %x:%x\n",
            cpuid(), tf->cs, tf->eip);
    lapiceoi();
    break;


  default:
    if(myproc() == 0 || (tf->cs&3) == 0){

      cprintf("unexpected trap %d from cpu %d eip %x (cr2=0x%x)\n",
              tf->trapno, cpuid(), tf->eip, rcr2());
      panic("trap");
    }

    cprintf("pid %d %s: trap %d err %d on cpu %d "
            "eip 0x%x addr 0x%x--kill proc\n",
            myproc()->pid, myproc()->name, tf->trapno,
            tf->err, cpuid(), tf->eip, rcr2());
    myproc()->killed = 1;
  }




  if(myproc() && myproc()->killed && (tf->cs&3) == 0x3)
    exit();



  if(myproc() && myproc()->state == RUNNING &&
     tf->trapno == 32 +0)
    yield();


  if(myproc() && myproc()->killed && (tf->cs&3) == 0x3)
    exit();
}
