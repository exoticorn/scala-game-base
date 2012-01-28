package de.exoticorn.resources

import java.net.URL
import scala.reflect.Manifest

trait Resource {
  def dispose() {}
}

class ResourceManager {
  var children = List.empty[ResourceManager]

  var resources = Map.empty[(Manifest[_], Any), Resource]

  def get[A <: Resource, B](resourceFactory: (ResourceManager, B) => A, resourceKey: B)(implicit m: Manifest[A]): A = {
    val key = (m, resourceKey)
    if (resources.isDefinedAt(key)) resources(key).asInstanceOf[A]
    else {
      val resource = resourceFactory(this, resourceKey)

      resources += key -> resource

      resource
    }
  }

  def get[A <: Resource](resourceFactory: (ResourceManager, URL) => A, path: String)(implicit m: Manifest[A]): A = {
    val url = getClass.getResource(path)

    if (url == null) {
      throw new Exception("Unable to find resource '" + path + "'")
    }

    get(resourceFactory, url)
  }

  def dispose() {
    for (child <- children) {
      child.dispose()
    }
    children = Nil
    for ((_, res) <- resources) {
      res.dispose()
    }
    resources = Map.empty
  }

  def newChild(): ResourceManager = {
    val rm = new ResourceManager
    children ::= rm
    rm
  }

  def newChild(f: ResourceManager => Unit) {
    val rm = newChild
    f(rm)
    children = children.filterNot(_ == rm)
  }
}

object ResourceManager {
  def root(f: ResourceManager => Unit) {
    val rm = new ResourceManager
    f(rm)
    rm.dispose()
  }
}