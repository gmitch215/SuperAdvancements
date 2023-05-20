package me.gamercoder215.superadvancements.v1_16_R2;

import me.gamercoder215.superadvancements.advancement.criteria.ACriteriaProgress;
import net.minecraft.server.v1_16_R2.CriterionProgress;
import org.jetbrains.annotations.Nullable;

import java.util.Date;

final class ACriteriaProgress1_16_R2 implements ACriteriaProgress {

    private final CriterionProgress handle;

    ACriteriaProgress1_16_R2(CriterionProgress handle) {
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
