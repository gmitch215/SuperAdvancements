import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

dependencies {
    compileOnly("org.spigotmc:spigot-api:1.12.2-R0.1-SNAPSHOT")
}

description = "Base Abstract for the SuperAdvancements API, ready for Implementation"

tasks {
    javadoc {
        sourceSets["main"].allJava.srcDir("src/main/javadoc")
    }

    register("sourcesJar", Jar::class.java) {
        dependsOn("classes")

        archiveFileName.set("SuperAdvancements-API-${project.version}-sources.jar")
        from(sourceSets["main"].allSource)
    }

    register("javadocJar", Jar::class.java) {
        dependsOn("javadoc")

        archiveFileName.set("SuperAdvancements-API-${project.version}-javadoc.jar")
        from(javadoc.get().destinationDir)
    }

    withType<ShadowJar> {
        dependsOn("sourcesJar", "javadocJar")
        archiveFileName.set("SuperAdvancements-API-${project.version}.jar")
    }
}

artifacts {
    add("archives", tasks["sourcesJar"])
    add("archives", tasks["javadocJar"])
}