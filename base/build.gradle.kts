import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

dependencies {
    compileOnly("org.spigotmc:spigot-api:1.12.2-R0.1-SNAPSHOT")
}

description = "Base Abstract for the SuperAdvancements API, ready for Implementation"

java {
    withJavadocJar()
    withSourcesJar()
}

tasks {
    javadoc {
        sourceSets["main"].allJava.srcDir("src/main/javadoc")
    }

    withType<ShadowJar> {
        dependsOn("sourcesJar", "javadocJar")
    }
}