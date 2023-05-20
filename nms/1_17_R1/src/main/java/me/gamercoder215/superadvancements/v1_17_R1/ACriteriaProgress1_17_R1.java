package me.gamercoder215.superadvancements.v1_17_R1;

import me.gamercoder215.superadvancements.advancement.criteria.ACriteriaProgress;
import net.minecraft.advancements.CriterionProgress;
import org.jetbrains.annotations.Nullable;

import java.util.Date;

final class ACriteriaProgress1_17_R1 implements ACriteriaProgress {

    private final CriterionProgress handle;

    ACriteriaProgress1_17_R1(CriterionProgress handle) {
        this.handle = handle;
    }

    @Override
    public @Nullable Date getObtained() {
        return handle.getDate();
    }

    @Override
    public boolean isDone() {
        return handle.a();
    }

    @Override
    public void grant() {
        handle.b();
    }

    @Override
    public void revoke() {
        handle.c();
    }
}