import org.openqa.selenium.{By, WebDriver}
import org.scalatest.Matchers
import By._


object Reader {
  
  def unit[R, S, A](a: A): Reader[R, S, A] =
    Reader((r, s) => (a, s))

  def sequenceViaFoldRight[R,S,A](sas: List[Reader[R,S, A]]): Reader[R,S, List[A]] =
    sas.foldRight(unit[R, S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def modify[R,S](f: S => S): Reader[R, S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[R, S]: Reader[R, S, S] = Reader((r, s) => (s, s))

  def set[R, S](s: S): Reader[R, S, Unit] = Reader((_,_) => ((), s))
}
case class Reader[R, S, +A](run: (R, S) => (A, S)) {

  def map[B](f: A => B): Reader[R, S, B] =
    flatMap(a => Reader.unit(f(a)))

  def map2[B,C](sb: Reader[R, S, B])(f: (A, B) => C): Reader[R, S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => Reader[R, S, B]): Reader[R, S, B] = Reader((r, s) => {
    val (a, s1) = run(r, s)
    f(a).run(r, s1)
  })

}

object WebReader {

  def unit[A](a: A): WebReader[A] =
    WebReader((r, s) => (a, s))

}

case class WebReader[A](override val run: (WebDriver, Pages) => (A, Pages)) extends Reader[WebDriver, Pages, A](run) {

  override def map[B](f: A => B): WebReader[B] =
    flatMap((a: A) => WebReader.unit(f(a)))

  def flatMap[B](f: A => WebReader[B]): WebReader[B] = WebReader((r, s) => {
    val (a, s1) = run(r, s)
    f(a).run(r, s1)
  })

}

object ElementReader {

  def unit[A](a: A): ElementReader[A] =
    ElementReader(_ => a)

}

case class ElementReader[A](val runElement: WebDriver => A) extends WebReader[A]((driver, p) => (runElement(driver), p)) {

  override def map[B](f: A => B): ElementReader[B] =
    flatMap((a: A) => ElementReader.unit(f(a)))

  def flatMap[B](f: A => ElementReader[B]): ElementReader[B] = ElementReader((r) => {
    val (a, _) = runElement(r)
    f(a).runElement(r)
  })

  def toPageReader = new PageReader((driver: WebDriver, pages: Pages) => (runElement(driver), pages))

}

object StepsReader {

  def unit[A](page: Any): StepsReader =
    StepsReader({case (driver, pages: Pages) => (pages + ("TODO" -> page))})

}

case class StepsReader(val runSteps: Pages => Unit) extends WebReader[Unit]((driver, p) => {runSteps(p); ((), p)}) {
  // for toStepsReader only
  def this(run: (WebDriver, Pages) => Pages) = this(run)

  // does map even make sense?
//  override def map(f: Unit => Any): StepsReader =
//    flatMap((a: Unit) => StepsReader.unit(f()))

  def flatMap(f: Unit => StepsReader): StepsReader = StepsReader((pages) => {
    runSteps(pages)
    f().runSteps(pages)
  })

}

type Pages = (Option[ExamplePage], Option[ExamplePage])

object PageReader {

  def unit(pages: Pages): PageReader[Unit] =
    PageReader(_ => ((), pages))

}

case class PageReader[A](val runPage: Pages => (A, Pages)) extends WebReader[A]((driver, pages) => runPage(pages)) {
  // for toPageReader only
  def this(run: (WebDriver, Pages) => (A, Pages)) = this(run)

//  override def map[B](f: A => B): PageReader[B] =
//    flatMap((a: A) => PageReader.unit(f(a)))

  def flatMap[B](f: A => PageReader[B]): PageReader[B] = PageReader((pages) => {
    val (a, newPages) = runPage(pages)
    f(a).runPage(newPages)
  })

  def addPage(pages: Pages) = {
    flatMap((a: A) => PageReader((oldP: Pages) => (a, (pages._1.orElse(oldP._1), pages._2.orElse(oldP._2)))))
  }

//  def removePage(page: Any) = {
//    flatMap((a: A) => PageReader(p => (a, p - ("TODO"))))
//  }

  def toStepsReader = new StepsReader((driver: WebDriver, pages: Pages) => runPage(pages)._2)

}

class ExamplePage {

  def root = ElementReader(_.findElement(xpath("/*")))

  def theButton = root.map(_.findElement(xpath("./*")))

  def textField = root.map(_.findElement(xpath("./*")))

  def link = root.map(_.findElement(xpath("./*")))

  def fillInText(input: String): PageReader[Nothing] = {
    (for {
      _ <- textField.map(_.sendKeys(input))
      _ <- theButton.map(_.click())
    } yield ()).toPageReader
  }

  def getText(): PageReader[String] =
    (for {
      entered <- textField.map(_.getText)
    } yield entered).toPageReader

  def followLink(): PageReader[Unit] =
    (for {
    _ <- link.map(_.click())
  } yield ()).toPageReader.addPage((None, Some(new ExamplePage)))

}

class ExampleSteps extends Matchers {

  def compound(): StepsReader = {
//    val page = new ExamplePage
    for {
      pages <- PageReader.unit((Some(new ExamplePage), None))
      pages <- pages._1.fillInText("test text")
      _ <- page.getText().map(_ should be ("test text"))
      another <- page.followLink()
    } yield another
  }

}