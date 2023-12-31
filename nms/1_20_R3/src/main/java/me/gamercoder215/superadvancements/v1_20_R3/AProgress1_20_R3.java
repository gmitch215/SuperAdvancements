package me.gamercoder215.superadvancements.v1_20_R3;

import me.gamercoder215.superadvancements.advancement.AProgress;
import me.gamercoder215.superadvancements.advancement.criteria.ACriteriaProgress;
import net.minecraft.advancements.AdvancementProgress;
import net.minecraft.advancements.CriterionProgress;
import net.minecraft.server.PlayerAdvancements;
import net.minecraft.server.level.ServerPlayer;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.stream.Collectors;

import static me.gamercoder215.superadvancements.v1_20_R3.Wrapper1_20_R3.toNMS;

@SuppressWarnings("unchecked")
final class AProgress1_20_R3 implements AProgress {

    private final Player p;
    private final ServerPlayer sp;
    private final PlayerAdvancements manager;

    private final net.minecraft.advancements.AdvancementHolder advancement;
    private final AdvancementProgress handle;

    AProgress1_20_R3(Player p, net.minecraft.advancements.AdvancementHolder advancement, AdvancementProgress handle) {
        this.p = p;
        this.sp = toNMS(p);
        this.manager = sp.getAdvancements();

        this.advancement = advancement;
        this.handle = handle;
    }

    @Override
    public @NotNull Player getPlayer() {
        return p;
    }

    @Override
    public boolean grant() {
        getRemainingCriteria().keySet().forEach(s -> manager.award(advancement, s));
        manager.flushDirty(sp);
        manager.save();
        return true;
    }

    @Override
    public boolean revoke() {
        getAwardedCriteria().keySet().forEach(s -> manager.revoke(advancement, s));
        manager.flushDirty(sp);
        manager.save();
        return true;
    }

    @Override
    public boolean isDone() {
        return handle.isDone();
    }

    @Override
    public @NotNull Map<String, ACriteriaProgress> getCriteria() {
        try {
            Field criteriaF = AdvancementProgress.class.getDeclaredField("e");
            criteriaF.setAccessible(true);
            Map<String, CriterionProgress> criteria = (Map<String, CriterionProgress>) criteriaF.get(handle);
            return criteria.entrySet()
                    .stream().collect(Collectors.toMap(Map.Entry::getKey, e -> new ACriteriaProgress1_20_R3(e.getValue())));
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public float getPercentageCompleted() {
        return handle.getPercent();
    }

    @Override
    public @Nullable String getProgressText() {
        return handle.getProgressText().getString();
    }

    @Override
    public boolean grantCriteria(@NotNull String name) {
        return handle.grantProgress(name);
    }

    @Override
    public boolean revokeCriteria(@NotNull String name) {
        return handle.revokeProgress(name);
    }

}
