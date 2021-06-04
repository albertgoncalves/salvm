#include "str.hpp"

String get_inst_tag_as_string(InstTag tag) {
    switch (tag) {
    case INST_HALT: {
        return TO_STR("halt");
    }

    case INST_PUSH: {
        return TO_STR("push");
    }

    case INST_TOP: {
        return TO_STR("top");
    }

    case INST_COPY: {
        return TO_STR("copy");
    }
    case INST_PUT: {
        return TO_STR("put");
    }

    case INST_CALL: {
        return TO_STR("call");
    }
    case INST_SCL: {
        return TO_STR("scl");
    }
    case INST_RET: {
        return TO_STR("ret");
    }

    case INST_BASE: {
        return TO_STR("base");
    }
    case INST_FRAME: {
        return TO_STR("frame");
    }
    case INST_RESET: {
        return TO_STR("reset");
    }

    case INST_RD8: {
        return TO_STR("rd8");
    }
    case INST_RD16: {
        return TO_STR("rd16");
    }
    case INST_RD32: {
        return TO_STR("rd32");
    }

    case INST_SV8: {
        return TO_STR("sv8");
    }
    case INST_SV16: {
        return TO_STR("sv16");
    }
    case INST_SV32: {
        return TO_STR("sv32");
    }

    case INST_JPZ: {
        return TO_STR("jpz");
    }
    case INST_JUMP: {
        return TO_STR("jump");
    }

    case INST_NOT: {
        return TO_STR("not");
    }
    case INST_EQ: {
        return TO_STR("eq");
    }

    case INST_ADDI: {
        return TO_STR("addi");
    }
    case INST_SUBI: {
        return TO_STR("subi");
    }
    case INST_MULI: {
        return TO_STR("muli");
    }
    case INST_DIVI: {
        return TO_STR("divi");
    }

    case INST_ADDF: {
        return TO_STR("addf");
    }
    case INST_SUBF: {
        return TO_STR("subf");
    }
    case INST_MULF: {
        return TO_STR("mulf");
    }
    case INST_DIVF: {
        return TO_STR("divf");
    }

    case INST_LTI: {
        return TO_STR("lti");
    }
    case INST_LEI: {
        return TO_STR("lei");
    }
    case INST_GTI: {
        return TO_STR("gti");
    }
    case INST_GEI: {
        return TO_STR("gei");
    }

    case INST_LTF: {
        return TO_STR("ltf");
    }
    case INST_LEF: {
        return TO_STR("lef");
    }
    case INST_GTF: {
        return TO_STR("gtf");
    }
    case INST_GEF: {
        return TO_STR("gef");
    }

    case INST_NATIVE: {
        return TO_STR("native");
    }

    case COUNT_INST_TAG:
    default: {
        ERROR();
    }
    }
}
