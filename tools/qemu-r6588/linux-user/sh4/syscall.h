struct target_pt_regs {
        unsigned long regs[16];
        unsigned long pc;
        unsigned long pr;
        unsigned long sr;
        unsigned long gbr;
        unsigned long mach;
        unsigned long macl;
        long tra;
};

#define UNAME_MACHINE "sh4"
