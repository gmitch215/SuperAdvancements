package me.gamercoder215.superadvancements.v1_20_R2;

import me.gamercoder215.superadvancements.advancement.criteria.ACriteriaProgress;
import net.minecraft.advancements.CriterionProgress;
import org.jetbrains.annotations.Nullable;

import java.util.Date;

final class ACriteriaProgress1_20_R2 implements ACriteriaProgress {

    private final CriterionProgress handle;

    ACriteriaProgress1_20_R2(CriterionProgress handle) {
        this.handle = handle;
    }

    @Override
    public @Nullable Date getObtained() {
        return Date.from(handle.getObtained());
    }

    @Override
    public boolean isDone() {
        return handle.isDone();
    }

    @Override
    public void grant() {
        handle.grant();
    }

    @Override
    public void revoke() {
        handle.revoke();
    }
}
