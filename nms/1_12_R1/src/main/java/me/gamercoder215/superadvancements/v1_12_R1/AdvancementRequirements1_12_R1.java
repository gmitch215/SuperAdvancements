package me.gamercoder215.superadvancements.v1_12_R1;

import java.util.Collection;
import java.util.Iterator;

// Copied from Decompiled NMS Code
@FunctionalInterface
interface AdvancementRequirements1_12_R1 {

    AdvancementRequirements1_12_R1 AND = s -> {
        String[][] requirements = new String[s.size()][];

        int i = 0;
        String req;

        for (Iterator<String> it = s.iterator(); it.hasNext(); requirements[i++] = new String[]{req})
            req = it.next();

        return requirements;
    };

    AdvancementRequirements1_12_R1 OR = s -> new String[][]{s.toArray(new String[0])};

    String[][] createRequirements(Collection<String> var1);

}
