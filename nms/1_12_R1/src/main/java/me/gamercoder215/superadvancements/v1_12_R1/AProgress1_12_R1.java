package me.gamercoder215.superadvancements.v1_12_R1;

import me.gamercoder215.superadvancements.advancement.AProgress;
import me.gamercoder215.superadvancements.advancement.criteria.ACriteriaProgress;
import net.minecraft.server.v1_12_R1.AdvancementDataPlayer;
import net.minecraft.server.v1_12_R1.AdvancementProgress;
import net.minecraft.server.v1_12_R1.CriterionProgress;
import net.minecraft.server.v1_12_R1.EntityPlayer;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.stream.Collectors;

import static me.gamercoder215.superadvancements.v1_12_R1.Wrapper1_12_R1.toNMS;

@SuppressWarnings("unchecked")
final class AProgress1_12_R1 implements AProgress {

    private final Player p;
    private final EntityPlayer sp;
    private final AdvancementDataPlayer manager;

    private final net.minecraft.server.v1_12_R1.Advancement advancement;
    private final AdvancementProgress handle;

    AProgress1_12_R1(Player p, net.minecraft.server.v1_12_R1.Advancement advancement, AdvancementProgress handle) {
        this.p = p;
        this.sp = toNMS(p);
        this.manager = sp.getAdvancementData();

        this.advancement = advancement;
        this.handle = handle;
    }

    @Override
    public @NotNull Player getPlayer() {
        return p;
    }

    @Override
    public boolean grant() {
        getRemainingCriteria().keySet().forEach(s ->
                manager.grantCriteria(advancement, s)
        );
        manager.b(sp);
        manager.b();
        return true;
    }

    @Override
    public boolean revoke() {
        getAwardedCriteria().keySet().forEach(s ->
                manager.grantCriteria(advancement, s)
        );
        manager.b(sp);
        manager.b();
        return true;
    }

    @Override
    public boolean isDone() {
        return handle.isDone();
    }

    @Override
    public @NotNull Map<String, ACriteriaProgress> getCriteria() {
        try {
            Field criteriaF = AdvancementProgress.class.getDeclaredField("a");
            criteriaF.setAccessible(true);
            Map<String, CriterionProgress> criteria = (Map<String, CriterionProgress>) criteriaF.get(handle);
            return criteria.entrySet()
                    .stream().collect(Collectors.toMap(Map.Entry::getKey, e -> new ACriteriaProgress1_12_R1(e.getValue())));
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    private String[][] getRequirements() {
        try {
            Field requirementsF = AdvancementProgress.class.getDeclaredField("b");
            requirementsF.setAccessible(true);
            return (String[][]) requirementsF.get(handle);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    private int countCompletedRequirements() {
        int count = 0;
        String[][] req = getRequirements();

        for (String[] arr : req) {
            boolean done = false;
            for (String criteria : arr) {
                CriterionProgress prog = handle.getCriterionProgress(criteria);
                if (prog != null && prog.a()) {
                    done = true;
                    break;
                }
            }

            if (done) ++count;
        }

        return count;
    }

    @Override
    public float getPercentageCompleted() {
        return (float) countCompletedRequirements() / getRequirements().length;
    }

    @Override
    public @Nullable String getProgressText() {
        return countCompletedRequirements() + " / " + getRequirements().length;
    }

    @Override
    public boolean grantCriteria(@NotNull String name) {
        return handle.a(name);
    }

    @Override
    public boolean revokeCriteria(@NotNull String name) {
        return handle.b(name);
    }

}
